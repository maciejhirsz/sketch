// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! # Utilities for conditional rendering
//!
//! The [`view!`](crate::view) macro produces unique transient types, so you might run into compile errors when branching:
//!
//! ```compile_fail
//! # use kobold::prelude::*;
//! #[component]
//! fn Conditional(illuminatus: bool) -> impl View {
//!     if illuminatus {
//!         view! { <p>"It was the year when they finally immanentized the Eschaton."</p> }
//!     } else {
//!         view! { <blockquote>"It was love at first sight."</blockquote> }
//!     }
//! }
//! ```
//!
//! Here Rust will inform you that:
//!
//! ```text
//! /     if illuminatus {
//! |         view! { <p>"It was the year when they finally immanentized the Eschaton."</p> }
//! |         ------------------------------------------------------------------------------- expected because of this
//! |     } else {
//! |         view! { <blockquote>"It was love at first sight."</blockquote> }
//! |         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ expected struct `Conditional::render::Transient`, found a different struct `Conditional::render::Transient`
//! |     }
//! |_____- `if` and `else` have incompatible types
//! ```
//!
//! While both types are _named_ `Transient`, they are in fact different types defined inline by the macro.
//!
//! In most cases all you have to do is annotate such component with [`#[component(auto_branch)]`](crate::component#componentauto_branch):
//!
//! ```
//! # use kobold::prelude::*;
//! #[component(auto_branch)]
//! fn Conditional(illuminatus: bool) -> impl View {
//!     if illuminatus {
//!         view! { <p>"It was the year when they finally immanentized the Eschaton."</p> }
//!     } else {
//!         view! { <blockquote>"It was love at first sight."</blockquote> }
//!     }
//! }
//! ```
//!
//! This flag is not enabled by default, yet, as there might be situations [`auto_branch`](crate::component#componentauto_branch)
//! doesn't handle correctly.
//!
//! ## Manual branching
//!
//! An always safe if more laborious way is to manually use one of the [`BranchN` enums](self#enums) from this module:
//!
//! ```
//! # use kobold::prelude::*;
//! use kobold::branching::Branch2;
//!
//! #[component]
//! fn Conditional(illuminatus: bool) -> impl View {
//!     if illuminatus {
//!         Branch2::A(view! {
//!             <p>"It was the year when they finally immanentized the Eschaton."</p>
//!         })
//!     } else {
//!         Branch2::B(view! {
//!             <blockquote>"It was love at first sight."</blockquote>
//!         })
//!     }
//! }
//! ```
//!
//! This is in fact all that the [`auto_branch`](crate::component#componentauto_branch) flag does for you automatically.
//!
//! For simple optional renders you can always use the standard library [`Option`](Option):
//!
//! ```
//! # use kobold::prelude::*;
//! #[component]
//! fn Conditional(illuminatus: bool) -> impl View {
//!     if illuminatus {
//!         Some(view! {
//!             <p>"It was the year when they finally immanentized the Eschaton."</p>
//!         })
//!     } else {
//!         None
//!     }
//! }
//! ```

use std::pin::Pin;

use wasm_bindgen::JsValue;
use web_sys::Node;

use crate::dom::{self, Anchor};
use crate::internal::{Field, Mut, Pre};
use crate::{Mountable, View};

macro_rules! branch {
    ($name:ident < $($var:ident),* >) => {
        pub enum $name<$($var),*> {
            $(
                $var($var),
            )*
        }

        impl<$($var),*> View for $name<$($var),*>
        where
            $(
                $var: View,
            )*
        {
            type Product = $name<$(Field<$var::Product>),*>;

            fn build(self, p: Pre<Self::Product>) -> Mut<Self::Product> {
                match self {
                    $(
                        $name::$var(html) => {
                            let p = p.put($name::$var(unsafe { Field::uninit() })).get_mut();

                            if let $name::$var(field) = p {
                                field.init(move |p| html.build(p));
                            }

                            Pin::new(p)
                        },
                    )*
                }
            }

            fn update(self, p: &mut Self::Product) {
                match (self, p) {
                    $(
                        ($name::$var(html), $name::$var(p)) => html.update(p),
                    )*

                    (html, p) => {
                        let old = Pre::replace(p, move |p| html.build(p));

                        old.replace_with(p.js());
                    }
                }
            }
        }

        impl<$($var),*> Mountable for $name<$(Field<$var>),*>
        where
            $(
                $var: Mountable,
            )*
        {
            type Js = Node;

            fn js(&self) -> &JsValue {
                match self {
                    $(
                        $name::$var(p) => p.js(),
                    )*
                }
            }

            fn replace_with(&self, new: &JsValue) {
                match self {
                    $(
                        $name::$var(p) => p.replace_with(new),
                    )*
                }
            }

            fn unmount(&self) {
                match self {
                    $(
                        $name::$var(p) => p.unmount(),
                    )*
                }
            }
        }

    };
}

branch!(Branch2<A, B>);
branch!(Branch3<A, B, C>);
branch!(Branch4<A, B, C, D>);
branch!(Branch5<A, B, C, D, E>);
branch!(Branch6<A, B, C, D, E, F>);
branch!(Branch7<A, B, C, D, E, F, G>);
branch!(Branch8<A, B, C, D, E, F, G, H>);
branch!(Branch9<A, B, C, D, E, F, G, H, I>);

pub struct EmptyNode(Node);

pub struct Empty;

impl Anchor for EmptyNode {
    type Js = Node;
    type Target = Node;

    fn anchor(&self) -> &Node {
        &self.0
    }
}

impl View for Empty {
    type Product = EmptyNode;

    fn build(self, p: Pre<EmptyNode>) -> Mut<EmptyNode> {
        p.put(EmptyNode(dom::empty_node()))
    }

    fn update(self, _: &mut EmptyNode) {}
}

impl<T: View> View for Option<T> {
    type Product = Branch2<Field<T::Product>, Field<EmptyNode>>;

    fn build(self, p: Pre<Self::Product>) -> Mut<Self::Product> {
        match self {
            Some(html) => {
                let p = p.put(Branch2::A(unsafe { Field::uninit() })).get_mut();

                if let Branch2::A(uninit) = p {
                    uninit.init(move |p| html.build(p));
                }

                Pin::new(p)
            }
            None => {
                p.put(Branch2::B(unsafe {
                    Field::new(EmptyNode(dom::empty_node()))
                }))
            }
        }
    }

    fn update(self, p: &mut Self::Product) {
        match (self, p) {
            (Some(html), Branch2::A(p)) => html.update(p),
            (None, Branch2::B(_)) => (),

            (html, p) => {
                let old = Pre::replace(p, move |p| html.build(p));

                old.replace_with(p.js());
            }
        }
    }
}
