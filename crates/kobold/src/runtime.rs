// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::cell::Cell;

use wasm_bindgen::JsValue;
use web_sys::Event;

use crate::{internal, Mountable, View};

mod ctx;

use ctx::EventCtx;

pub use ctx::EventContext;

struct RuntimeData<P, U> {
    product: P,
    update: U,
}

trait Runtime {
    fn update(&mut self, ctx: Option<&mut EventCtx>);
}

impl<P, U> Runtime for RuntimeData<P, U>
where
    P: Trigger,
    U: Fn(&mut P),
{
    fn update(&mut self, ctx: Option<&mut EventCtx>) {
        let p = &mut self.product;

        if let Some(ctx) = ctx {
            if let Some(Then::Stop) = p.trigger(ctx) {
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
    #[inline]
    pub fn reserve(n: u32) -> Self {
        let id = EVENT_ID.get();

        EVENT_ID.set(id + n);

        EventId(id)
    }

    #[inline]
    pub fn offset(self, offset: u32) -> Self {
        EventId(self.0 + offset)
    }

    #[inline]
    pub fn js_handler(self) -> JsValue {
        internal::make_event_handler(self.0)
    }
}

pub trait Trigger {
    fn trigger<C: EventContext>(&mut self, _: &mut C) -> Option<Then> {
        None
    }
}

mod turbo_batch {
    use super::EventId;

    trait Turbo {
        type Combine<T: Turbo>: Turbo;

        fn make() -> Self;

        // fn combine<T: Turbo>() -> Self::Combine<T> {
        //     Self::Combine::<T>::make()
        // }
    }

    impl Turbo for () {
        type Combine<T: Turbo> = T;

        fn make() -> Self {
            ()
        }
    }

    impl Turbo for EventId {
        type Combine<T: Turbo> = EventId;

        fn make() -> Self {
            EventId(0)
        }
    }

    macro_rules! combine {
        ($a:ty, $b:ty, $($cont:tt)+) => {
            combine!(combine!($a, $b), $($cont)*)
        };

        ($a:ty, $b:ty) => {
            <$a as Turbo>::Combine<$b>
        };

        ($a:ty) => {$a}
    }

    struct Foo<A: View, B: View>(A, B);

    trait View {
        type Tur: Turbo;
    }

    impl<A: View, B: View> View for Foo<A, B> {
        type Tur = combine!(A::Tur, B::Tur, EventId);
    }

    fn foo() {
        // let _: () = <<<() as Turbo>::Combine<()> as Turbo>::Combine<()> as Turbo>::Combine::<()>::make();
        let _: () = <combine!((), (), (), (), ())>::make();
        let _: EventId = <combine!(EventId, (), (), (), ())>::make();
        let _: EventId = <combine!((), (), (), (), EventId)>::make();
        let _: EventId = <combine!((), (), EventId, (), ())>::make();

        let _: () = <combine!((), ())>::make();
        let _: EventId = <combine!((), EventId)>::make();
        let _: EventId = <combine!(EventId, ())>::make();
        let _: EventId = <combine!(EventId, EventId)>::make();
    }
}

/// Start the Kobold app by mounting given [`View`] in the document `body`.
pub fn start<F, V>(render: F)
where
    F: Fn() -> V + 'static,
    V: View,
{
    if INIT.get() {
        return;
    }
    INIT.set(true);

    init_panic_hook();

    let runtime = Box::new(RuntimeData {
        product: render().build(),
        update: move |p: &mut V::Product| render().update(p),
    });

    internal::append_body(runtime.product.js());

    RUNTIME.set(Some(Box::leak(runtime)));
}

pub(crate) fn trigger(eid: EventId, event: Event) {
    if let Some(runtime) = RUNTIME.take() {
        let mut ctx = EventCtx::new(eid, &event);

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
