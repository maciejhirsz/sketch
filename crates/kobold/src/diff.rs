// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Utilities for diffing values in render functions.

use std::ops::Deref;
use std::ptr::NonNull;

use web_sys::Node;

use crate::attribute::Attribute;
use crate::dom::{Anchor, TextContent};
use crate::runtime::{Context, EventContext, Then, Trigger};
use crate::value::{IntoText, Value};
use crate::{Mountable, View};

mod ver;

pub use ver::Ver;

/// Create a wrapper around a `view` that will prevent updates to it, unless
/// the value of `guard` has changed.
///
/// Fencing against updates can be a great optimization that combines well
/// with the [`use`](crate::keywords::use) keyword.
///
/// ```
/// use kobold::prelude::*;
/// use kobold::diff::fence;
///
/// struct User {
///     id: usize,
///     name: String,
///     email: String,
/// }
///
/// #[component]
/// fn user_row(user: &User) -> impl View + '_ {
///     fence(user.id, || view! {
///         // This row is only re-rendered if `user.id` has changed
///         <tr>
///             <td>{ user.id }</td>
///
///             // Assuming that `name` never changes for a `User`
///             // we can disable diffing here with the `use` keyword.
///             <td>{ use &user.name }</td>
///             <td>{ use &user.email }</td>
///         </tr>
///     })
/// }
/// # fn main() {}
/// ```
pub const fn fence<D, V, F>(guard: D, render: F) -> Fence<D, F>
where
    D: Diff,
    V: View,
    F: FnOnce() -> V,
{
    Fence {
        guard,
        inner: render,
    }
}

/// Smart [`View`] that guards against unnecessary renders, see [`fence`].
pub struct Fence<D, F> {
    guard: D,
    inner: F,
}

impl<D, F, V> View for Fence<D, F>
where
    D: Diff,
    F: FnOnce() -> V,
    V: View,
{
    type Product = Fence<D::Memo, V::Product>;

    fn build<C: Context>(self, ctx: C) -> Self::Product {
        Fence {
            guard: self.guard.into_memo(),
            inner: (self.inner)().build(ctx),
        }
    }

    fn update<C: Context>(self, ctx: C, p: &mut Self::Product) {
        if self.guard.diff(&mut p.guard) {
            (self.inner)().update(ctx, &mut p.inner);
        }
    }
}

impl<D, P> Anchor for Fence<D, P>
where
    P: Mountable,
{
    type Js = P::Js;
    type Target = P;

    fn anchor(&self) -> &P {
        &self.inner
    }
}

impl<D, P> Trigger for Fence<D, P>
where
    P: Trigger,
{
    fn trigger<C: EventContext>(&mut self, ctx: &mut C) -> Option<Then> {
        self.inner.trigger(ctx)
    }
}

/// Trait that defines how different values can be _diffed_ at runtime.
pub trait Diff: Copy {
    /// Data used to check if current value is different from the one in the past.
    type Memo: 'static;

    /// Generate a new `Memo` for this type.
    fn into_memo(self) -> Self::Memo;

    /// Diff current value against the `Memo`, update it if necessary and return
    /// `true` if it has changed.
    fn diff(self, memo: &mut Self::Memo) -> bool;
}

impl<T> Diff for Option<T>
where
    T: Copy + Eq + 'static,
{
    type Memo = Self;

    fn into_memo(self) -> Self {
        self
    }

    fn diff(self, memo: &mut Self) -> bool {
        if self != *memo {
            *memo = self;
            true
        } else {
            false
        }
    }
}

macro_rules! impl_diff_str {
    ($(&$ty:ty),*) => {
        $(
            impl Diff for &$ty {
                type Memo = NonNull<()>;

                fn into_memo(self) -> Self::Memo {
                    NonNull::from(AsRef::<str>::as_ref(self)).cast()
                }

                fn diff(self, memo: &mut Self::Memo) -> bool {
                    let new = NonNull::from(AsRef::<str>::as_ref(self)).cast();

                    if new != *memo {
                        *memo = new;
                        true
                    } else {
                        false
                    }
                }
            }
        )*
    };
}

macro_rules! impl_diff_val {
    ($($ty:ty),*) => {
        $(
            impl Diff for $ty {
                type Memo = $ty;

                fn into_memo(self) -> $ty {
                    self
                }

                fn diff(self, memo: &mut $ty) -> bool {
                    if self != *memo {
                        *memo = self;
                        true
                    } else {
                        false
                    }
                }
            }
        )*
    };
}

impl_diff_str!(&str, &String);
impl_diff_val!(bool, u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize, f32, f64);

/// Smart [`View`] that never performs diffing and instead always triggers
/// updates.
///
/// See [`use`](crate::keywords::use)
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Eager<T>(pub(crate) T);

/// Smart [`View`] that never performs diffing and instead never triggers
/// updates.
///
/// See [`static`](crate::keywords::static)
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Static<T>(pub(crate) T);

macro_rules! impl_no_diff {
    ($name:ident, $update:expr) => {
        impl<T> Deref for $name<T> {
            type Target = T;

            fn deref(&self) -> &T {
                &self.0
            }
        }

        impl<T> View for $name<T>
        where
            T: Value<TextContent> + IntoText + Copy,
        {
            type Product = Node;

            fn build<C: Context>(self, _: C) -> Node {
                self.into_text()
            }

            fn update<C: Context>(self, _: C, node: &mut Node) {
                if $update {
                    self.0.set_prop(TextContent, node);
                }
            }
        }

        impl<T, P> Attribute<P> for $name<T>
        where
            T: Value<P>,
        {
            type Product = ();

            fn build(self) {}

            fn build_in(self, prop: P, node: &Node) {
                self.0.set_prop(prop, node);
            }

            fn update_in(self, prop: P, node: &Node, _: &mut ()) {
                if $update {
                    self.0.set_prop(prop, node);
                }
            }
        }

        impl<T> Diff for $name<T>
        where
            T: Copy,
        {
            type Memo = ();

            fn into_memo(self) {}

            fn diff(self, _: &mut ()) -> bool {
                $update
            }
        }

        impl AsRef<str> for $name<&str> {
            fn as_ref(&self) -> &str {
                self.0
            }
        }

        impl View for $name<String> {
            type Product = Node;

            fn build<C: Context>(self, _: C) -> Self::Product {
                self.into_text()
            }

            fn update<C: Context>(self, _: C, p: &mut Self::Product) {
                if $update {
                    self.0.set_prop(TextContent, p);
                }
            }
        }
    };
}

impl_no_diff!(Eager, true);
impl_no_diff!(Static, false);
