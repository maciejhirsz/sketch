// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! # Utilities for building stateful views
//!
//! **Kobold** doesn't allocate any memory on the heap for its simple components, and there
//! is no way to update them short of the parent view re-rendering them.
//!
//! However a fully functional app like that wouldn't be very useful, as all it
//! could ever do is render itself once. To get around this the [`stateful`] function can
//! be used to create views that have ownership over some arbitrary mutable state.
//!
use wasm_bindgen::JsValue;

use crate::runtime::{EventContext, EventId, Then, Trigger};
use crate::{Mountable, View};

mod hook;
mod into_state;

pub use hook::{Bound, Hook, Signal};
pub use into_state::IntoState;

/// Create a stateful [`View`] over some mutable state. The state
/// needs to be created using the [`IntoState`] trait.
///
/// ```
/// # use::kobold::prelude::*;
/// // `IntoState` is implemented for primitive values
/// let int_view = stateful(0, |count: &Hook<i32>| { "TODO" });
///
/// // Another easy way to create arbitrary state is using a closure...
/// let string_view = stateful(|| String::from("foo"), |text: &Hook<String>| { "TODO" });
///
/// // ...or a function with no parameters
/// let vec_view = stateful(Vec::new, |counts: &Hook<Vec<i32>>| { "TODO" });
/// ```
pub fn stateful<'a, S, F, V>(
    state: S,
    render: F,
) -> Stateful<S, impl Fn(*const Hook<S::State>) -> V>
where
    S: IntoState,
    F: Fn(&'a Hook<S::State>) -> V,
    V: View + 'a,
{
    // There is no safe way to represent a generic closure with generic return type
    // that borrows from that closure's arguments, without also slapping a lifetime.
    //
    // The `stateful` function ensures that correct lifetimes are used before we
    // erase them for the use in the `Stateful` struct.
    let render = move |hook: *const Hook<S::State>| render(unsafe { &*hook });
    Stateful { state, render }
}

pub struct Stateful<S, F> {
    state: S,
    render: F,
}

pub struct StatefulProduct<S, P> {
    state: Hook<S>,
    product: P,
}

impl<S, F, V> View for Stateful<S, F>
where
    S: IntoState,
    F: Fn(*const Hook<S::State>) -> V,
    V: View,
{
    type Product = StatefulProduct<S::State, V::Product>;

    fn build(self) -> Self::Product {
        let state = Hook::new(self.state.init());
        let product = (self.render)(&state).build();

        StatefulProduct { state, product }
    }

    fn update(self, p: &mut Self::Product) {
        (self.render)(&p.state).update(&mut p.product)
    }
}

impl<S, P> Mountable for StatefulProduct<S, P>
where
    S: 'static,
    P: Mountable,
{
    type Js = P::Js;

    fn js(&self) -> &JsValue {
        self.product.js()
    }

    fn unmount(&self) {
        self.product.unmount()
    }

    fn replace_with(&self, new: &JsValue) {
        self.product.replace_with(new);
    }
}

impl<S, P> Trigger for StatefulProduct<S, P>
where
    S: 'static,
    P: Trigger,
{
    fn trigger<C: EventContext>(&mut self, ctx: &mut C) -> Option<Then> {
        let mut ctx = ctx.attach(&mut self.state);

        self.product.trigger(&mut ctx)
    }
}

impl<S, R> Stateful<S, R>
where
    S: IntoState,
{
    pub fn once<F, P>(self, handler: F) -> Once<S, R, F>
    where
        F: FnOnce(Signal<S::State>) -> P,
    {
        Once {
            with_state: self,
            handler,
        }
    }
}

pub struct Once<S, R, F> {
    with_state: Stateful<S, R>,
    handler: F,
}

pub struct OnceProduct<S, P, D> {
    eid: EventId,
    inner: StatefulProduct<S, P>,
    _no_drop: D,
}

impl<S, P, D> Mountable for OnceProduct<S, P, D>
where
    StatefulProduct<S, P>: Mountable,
    P: Trigger,
    D: 'static,
{
    type Js = <StatefulProduct<S, P> as Mountable>::Js;

    fn js(&self) -> &JsValue {
        self.inner.js()
    }

    fn unmount(&self) {
        self.inner.unmount()
    }

    fn replace_with(&self, new: &JsValue) {
        self.inner.replace_with(new);
    }
}

impl<S, P, D> Trigger for OnceProduct<S, P, D>
where
    S: 'static,
    P: Trigger,
{
    fn trigger<C: EventContext>(&mut self, ctx: &mut C) -> Option<Then> {
        let mut ctx = ctx.attach(&mut self.inner.state);

        if let Some(then) = ctx.try_signal::<S>(self.eid) {
            return Some(then);
        }

        self.inner.product.trigger(&mut ctx)
    }
}

impl<S, R, F, V, D> View for Once<S, R, F>
where
    S: IntoState,
    R: Fn(*const Hook<S::State>) -> V,
    F: FnOnce(Signal<S::State>) -> D,
    V: View,
    D: 'static,
{
    type Product = OnceProduct<S::State, V::Product, D>;

    fn build(self) -> Self::Product {
        let inner = self.with_state.build();
        let eid = EventId::reserve(1);

        let _no_drop = (self.handler)(Signal::new(eid));

        OnceProduct {
            eid,
            inner,
            _no_drop,
        }
    }

    fn update(self, p: &mut Self::Product) {
        self.with_state.update(&mut p.inner)
    }
}
