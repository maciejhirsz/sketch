// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Utilities for handling DOM events

use std::marker::PhantomData;
use std::ops::Deref;

use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen::{JsCast, JsValue};
use web_sys::{HtmlElement, HtmlInputElement};

use crate::internal;
use crate::runtime::{EventContext, EventId, Then};

#[wasm_bindgen]
extern "C" {
    type EventWithTarget;

    #[wasm_bindgen(method, getter)]
    fn target(this: &EventWithTarget) -> HtmlElement;

    #[wasm_bindgen(method, getter, js_name = "currentTarget")]
    fn current_target(this: &EventWithTarget) -> HtmlElement;
}

macro_rules! event {
    ($(#[doc = $doc:literal] $event:ident,)*) => {
        $(
            #[doc = concat!("Smart wrapper around a ", $doc, "which includes the type information of the event target")]
            #[repr(transparent)]
            pub struct $event<T> {
                event: web_sys::$event,
                _target: PhantomData<T>,
            }

            impl<T> EventCast for $event<T> {
                fn cast_from(e: &web_sys::Event) -> &Self {
                    unsafe { &*(e as *const _ as *const Self) }
                }
            }

            impl<T> Deref for $event<T> {
                type Target = web_sys::$event;

                fn deref(&self) -> &Self::Target {
                    &self.event.unchecked_ref()
                }
            }

            impl<T> $event<T> {
                /// Return a reference to the target element.
                ///
                /// This method shadows over the [`Event::target`](web_sys::Event::target)
                /// method provided by `web-sys` and makes it infallible.
                pub fn target(&self) -> HtmlElement {
                    self.event.unchecked_ref::<EventWithTarget>().target().unchecked_into()
                }

                /// Return a reference to the target element.
                ///
                /// This method shadows over the [`Event::target`](web_sys::Event::target)
                /// method provided by `web-sys` and makes it infallible.
                pub fn current_target(&self) -> EventTarget<T>
                where
                    T: JsCast,
                {
                    EventTarget(self.event.unchecked_ref::<EventWithTarget>().current_target().unchecked_into())
                }
            }
        )*
    };
}

mod sealed {
    pub trait EventCast {
        fn cast_from(e: &web_sys::Event) -> &Self;
    }

    impl EventCast for web_sys::Event {
        fn cast_from(e: &web_sys::Event) -> &Self {
            e
        }
    }
}

pub(crate) use sealed::EventCast;

event! {
    /// [`web_sys::Event`](web_sys::Event)
    Event,
    /// [`web_sys::KeyboardEvent`](web_sys::KeyboardEvent)
    KeyboardEvent,
    /// [`web_sys::MouseEvent`](web_sys::MouseEvent)
    MouseEvent,
}

pub trait Listener<E>
where
    E: EventCast,
    Self: Sized,
{
    fn build(self) -> EventId {
        EventId::next()
    }

    fn trigger(self, ctx: &EventContext, eid: EventId) -> Option<Then>;
}

impl<E, F> Listener<E> for F
where
    F: FnOnce(&E) + 'static,
    E: EventCast,
{
    fn trigger(self, ctx: &EventContext, eid: EventId) -> Option<Then> {
        ctx.get(eid).map(move |e| {
            self(E::cast_from(e));
            Then::Stop
        })
    }
}

pub trait ListenerHandle {
    fn js_value(&mut self) -> JsValue;
}

impl ListenerHandle for EventId {
    fn js_value(&mut self) -> JsValue {
        internal::make_event_handler(self.0)
    }
}

/// A wrapper over some event target type from web-sys.
#[repr(transparent)]
pub struct EventTarget<T>(T);

impl<T> Deref for EventTarget<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl EventTarget<HtmlInputElement> {
    pub fn focus(&self) {
        drop(self.0.focus());
    }
}
