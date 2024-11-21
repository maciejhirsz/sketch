// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::cell::Cell;
use std::future::Future;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::rc::{Rc, Weak};

use wasm_bindgen_futures::spawn_local;

use crate::event::{EventCast, Listener, ListenerHandle};
use crate::runtime::{EventContext, EventId, Then};
use crate::View;

type SignalUpdate<S> = Cell<Option<Box<dyn FnOnce(&mut S) -> Then>>>;

pub struct Signal<S> {
    eid: EventId,
    update: Weak<SignalUpdate<S>>,
}

impl<S> Signal<S> {
    pub(crate) fn new(eid: EventId, update: Weak<SignalUpdate<S>>) -> Self {
        Signal { eid, update }
    }

    /// Update the state behind this `Signal`.
    ///
    /// ```
    /// # use kobold::prelude::*;
    /// fn example(count: Signal<i32>) {
    ///     // increment count and trigger a render
    ///     count.update(|count| *count += 1);
    ///
    ///     // increment count if less than 10, only render on change
    ///     count.update(|count| {
    ///         if *count < 10 {
    ///             *count += 1;
    ///             Then::Render
    ///         } else {
    ///             Then::Stop
    ///         }
    ///     })
    /// }
    /// ```
    pub fn update<F, O>(&self, mutator: F)
    where
        F: FnOnce(&mut S) -> O + 'static,
        O: Into<Then>,
    {
        let Some(update) = self.update.upgrade() else {
            return;
        };

        update.set(Some(Box::new(move |state| mutator(state).into())));
    }

    /// Same as [`update`](Signal::update), but it never renders updates.
    pub fn update_silent<F>(&self, mutator: F)
    where
        F: FnOnce(&mut S) + 'static,
    {
        let Some(update) = self.update.upgrade() else {
            return;
        };

        update.set(Some(Box::new(move |state| {
            mutator(state);

            Then::Stop
        })));
    }

    /// Replace the entire state with a new value and trigger an update.
    pub fn set(&self, val: S)
    where
        S: 'static,
    {
        self.update(move |s| *s = val);
    }
}

pub struct Hook<S> {
    inner: S,
}

impl<S> Deref for Hook<S> {
    type Target = S;

    fn deref(&self) -> &S {
        &self.inner
    }
}

impl<S> DerefMut for Hook<S> {
    fn deref_mut(&mut self) -> &mut S {
        &mut self.inner
    }
}

impl<S> Hook<S> {
    pub(crate) fn new(inner: S) -> Self {
        Hook { inner }
    }

    /// Binds a closure to a mutable reference of the state. While this method is public
    /// it's recommended to use the [`bind!`](crate::bind) macro instead.
    pub fn bind<E, F, O>(&self, callback: F) -> Bound<S, F>
    where
        E: EventCast,
        F: Fn(&mut S, &E) -> O + 'static,
        O: Into<Then>,
    {
        Bound {
            callback,
            _marker: PhantomData,
        }
    }

    pub fn bind_async<E, F, T>(&self, callback: F) -> BoundAsync<S, F>
    where
        S: 'static,
        E: EventCast,
        F: Fn(Signal<S>, &E) -> T + 'static,
        T: Future<Output = ()> + 'static,
    {
        BoundAsync {
            callback,
            _marker: PhantomData,
        }
    }

    /// Get the value of state if state implements `Copy`. This is equivalent to writing
    /// `**hook` but conveys intent better.
    pub fn get(&self) -> S
    where
        S: Copy,
    {
        **self
    }
}

impl<'a, V> View for &'a Hook<V>
where
    &'a V: View + 'a,
{
    type Product = <&'a V as View>::Product;

    fn build(self) -> Self::Product {
        (**self).build()
    }

    fn update(self, p: &mut Self::Product) {
        (**self).update(p)
    }
}

#[derive(Clone, Copy)]
pub struct Bound<S, F> {
    callback: F,
    _marker: PhantomData<S>,
}

impl<E, S, F, O> Listener<E> for Bound<S, F>
where
    S: 'static,
    E: EventCast,
    F: Fn(&mut S, &E) -> O + 'static,
    O: Into<Then>,
{
    type Product = Self;

    fn build(self) -> Self {
        self
    }

    fn update(self, p: &mut Self) {
        p.callback = self.callback;
    }
}

impl<E, S, F, O> ListenerHandle<E> for Bound<S, F>
where
    S: 'static,
    E: EventCast,
    F: Fn(&mut S, &E) -> O + 'static,
    O: Into<Then>,
{
    fn trigger<C: EventContext>(&self, ctx: &mut C, eid: EventId) -> Option<Then> {
        ctx.with_state(eid, &self.callback)
    }
}

#[derive(Clone, Copy)]
pub struct BoundAsync<S, F> {
    callback: F,
    _marker: PhantomData<S>,
}

pub struct BoundAsyncProduct<S, F> {
    callback: F,
    update: Rc<SignalUpdate<S>>,
}

impl<E, S, F, T> ListenerHandle<E> for BoundAsyncProduct<S, F>
where
    S: 'static,
    E: EventCast,
    F: Fn(Signal<S>, &E) -> T + 'static,
    T: Future<Output = ()> + 'static,
{
    // fn update(self, p: &mut Self) {
    //     p.callback = self.callback;
    // }

    fn trigger<C: EventContext>(&self, ctx: &mut C, eid: EventId) -> Option<Then> {
        match self.update.take() {
            Some(update) => ctx.with_state(eid, move |state, _: &E| update(state)),
            None => Some(Then::Stop),
        };

        panic!();
        // ctx.event(eid).map(|event| {
        //     let fut = (self.callback)(Signal::new(eid), event);

        //     spawn_local(fut);

        //     Then::Stop
        // })
    }
}
