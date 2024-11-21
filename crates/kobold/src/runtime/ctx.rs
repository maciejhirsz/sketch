// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use web_sys::Event;

use crate::event::EventCast;
use crate::runtime::{EventId, Then};
use crate::state::Hook;

pub struct EventCtx<'a, T = ()> {
    eid: EventId,
    event: &'a Event,
    states: T,
}

impl<'event> EventCtx<'event> {
    pub fn new(eid: EventId, event: &'event Event) -> Self {
        EventCtx {
            eid,
            event,
            states: (),
        }
    }
}

pub trait ContextState {
    type Borrow<'b>: ContextState + 'b
    where
        Self: 'b;

    fn with_state<S, F>(&mut self, then: F) -> Option<Then>
    where
        S: 'static,
        F: FnOnce(&mut S) -> Then;

    fn borrow<'b>(&'b mut self) -> Self::Borrow<'b>;
}

impl ContextState for () {
    type Borrow<'b> = ();

    fn with_state<S, F>(&mut self, _: F) -> Option<Then>
    where
        F: FnOnce(&mut S) -> Then,
    {
        None
    }

    fn borrow<'b>(&'b mut self) -> () {
        ()
    }
}

impl<'a, T, U> ContextState for (&'a mut Hook<T>, U)
where
    T: 'static,
    U: ContextState,
{
    type Borrow<'b> = (&'b mut Hook<T>, U::Borrow<'b>)
    where
        Self: 'b;

    fn with_state<S, F>(&mut self, then: F) -> Option<Then>
    where
        S: 'static,
        F: FnOnce(&mut S) -> Then,
    {
        use std::any::TypeId;

        if TypeId::of::<T>() == TypeId::of::<S>() {
            // ⚠️ Safety:
            // ==========
            //
            // Both the `TypeId` check and the invariant nature of `EventId` always
            // pointing to the same type of a state give us a guarantee that we can
            // cast `&mut Hook<T>` into `&mut Hook<S>` as they are the same type.
            let cast_hook = unsafe { &mut *(self.0 as *mut Hook<T> as *mut Hook<S>) };

            return Some(then(cast_hook));
        }

        self.1.with_state(then)
    }

    fn borrow<'b>(&'b mut self) -> Self::Borrow<'b> {
        (self.0, self.1.borrow())
    }
}

pub trait EventContext {
    type Attached<'b, S>: EventContext + 'b
    where
        S: 'static,
        Self: 'b;

    fn event<E>(&self, eid: EventId) -> Option<&E>
    where
        E: EventCast;

    fn attach<'b, S>(&'b mut self, hook: &'b mut Hook<S>) -> Self::Attached<'b, S>
    where
        S: 'static,
        Self: 'b;

    fn with_state<S, E, F, O>(&mut self, eid: EventId, then: F) -> Option<Then>
    where
        S: 'static,
        E: EventCast,
        F: FnOnce(&mut S, &E) -> O,
        O: Into<Then>;
}

impl<'a, T> EventContext for EventCtx<'a, T>
where
    T: ContextState,
{
    type Attached<'b, S> = EventCtx<'b, (&'b mut Hook<S>, T::Borrow<'b>)>
    where
        S: 'static,
        Self: 'b;

    fn event<E>(&self, eid: EventId) -> Option<&E>
    where
        E: EventCast,
    {
        if eid == self.eid {
            Some(E::cast_from(&self.event))
        } else {
            None
        }
    }

    fn attach<'b, S>(&'b mut self, hook: &'b mut Hook<S>) -> Self::Attached<'b, S>
    where
        S: 'static,
        Self: 'b,
    {
        EventCtx {
            eid: self.eid,
            event: self.event,
            states: (hook, self.states.borrow()),
        }
    }

    fn with_state<S, E, F, O>(&mut self, eid: EventId, then: F) -> Option<Then>
    where
        S: 'static,
        E: EventCast,
        F: FnOnce(&mut S, &E) -> O,
        O: Into<Then>,
    {
        if eid != self.eid {
            return None;
        }

        let event = E::cast_from(&self.event);

        self.states
            .with_state(move |state| then(state, event).into())
    }
}
