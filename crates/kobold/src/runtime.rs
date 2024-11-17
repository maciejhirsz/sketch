// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::cell::Cell;

use web_sys::Event;

use crate::{internal, Mountable, View};

// mod ctx;

// use ctx::EventCtx;

// pub use ctx::EventContext;

pub struct EventContext {
    eid: EventId,
    event: Event,
}

impl EventContext {
    pub(crate) fn get(&self, eid: EventId) -> Option<&Event> {
        if self.eid == eid {
            Some(&self.event)
        } else {
            None
        }
    }
}

struct RuntimeData<P, T, U> {
    product: P,
    trigger: T,
    update: U,
}

trait Runtime {
    fn update(&mut self, ctx: Option<&EventContext>);
}

impl<P, T, U> Runtime for RuntimeData<P, T, U>
where
    T: Fn(&EventContext, &P) -> Option<Then>,
    U: Fn(&mut P),
{
    fn update(&mut self, ctx: Option<&EventContext>) {
        let p = &mut self.product;

        if let Some(ctx) = ctx {
            if let Some(Then::Stop) = (self.trigger)(ctx, p) {
                return;
            }
        }

        (self.update)(p);
    }
}

/// Describes whether or not a component should be rendered after state changes.
pub enum Then {
    /// This is a silent update
    Stop,
    /// Render the view after this update
    Render,
}

impl From<()> for Then {
    fn from(_: ()) -> Self {
        Then::Render
    }
}

thread_local! {
    static EVENT_ID: Cell<u32> = const { Cell::new(0) };

    static INIT: Cell<bool> = const { Cell::new(false) };

    static RUNTIME: Cell<Option<&mut dyn Runtime>> = const { Cell::new(None) };
}

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct EventId(pub(crate) u32);

impl EventId {
    pub(crate) fn next() -> Self {
        let id = EVENT_ID.get();

        EVENT_ID.set(id + 1);

        EventId(id)
    }
}

// pub trait Trigger {
//     fn trigger<C: EventContext>(&mut self, _: &mut C) -> Option<Then> {
//         None
//     }
// }

/// Start the Kobold app by mounting given [`View`] in the document `body`.
pub fn start<F, V>(render: F)
where
    F: Fn() -> V + Copy + 'static,
    V: View,
{
    if INIT.get() {
        return;
    }
    INIT.set(true);

    init_panic_hook();

    let runtime = Box::new(RuntimeData {
        product: render().build(),
        trigger: move |c: &EventContext, p: &V::Product| render().trigger(c, p),
        update: move |p: &mut V::Product| render().update(p),
    });

    internal::append_body(runtime.product.js());

    RUNTIME.set(Some(Box::leak(runtime)));
}

pub(crate) fn trigger(eid: EventId, event: Event) {
    if let Some(runtime) = RUNTIME.take() {
        let mut ctx = EventContext { eid, event };

        runtime.update(Some(&mut ctx));

        RUNTIME.set(Some(runtime));
    }
}

pub(crate) fn lock_update<F, R>(f: F)
where
    F: FnOnce() -> R,
    R: Into<Then>,
{
    if let Some(runtime) = RUNTIME.take() {
        if let Then::Render = f().into() {
            runtime.update(None);
        }

        RUNTIME.set(Some(runtime));
    }
}

fn init_panic_hook() {
    // Only enable console hook on debug builds
    #[cfg(debug_assertions)]
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));
}
