// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Utilities for rendering lists

use std::pin::Pin;

use web_sys::Node;

use crate::dom::{Anchor, Fragment, FragmentBuilder};
use crate::internal::{In, Out};
use crate::{Mountable, View};

/// Wrapper type that implements `View` for iterators, created by the
/// [`for`](crate::keywords::for) keyword.
#[repr(transparent)]
pub struct List<T>(pub(crate) T);

pub struct ListProduct<T> {
    list: Vec<Pin<Box<T>>>,
    fragment: FragmentBuilder,
}

impl<T> Anchor for ListProduct<T> {
    type Js = Node;
    type Target = Fragment;

    fn anchor(&self) -> &Fragment {
        &self.fragment
    }
}

impl<T> View for List<T>
where
    T: IntoIterator,
    <T as IntoIterator>::Item: View,
{
    type Product = ListProduct<<T::Item as View>::Product>;

    fn build(self, p: In<Self::Product>) -> Out<Self::Product> {
        let iter = self.0.into_iter();
        let fragment = FragmentBuilder::new();

        let list: Vec<_> = iter
            .map(|item| {
                let built = In::boxed(|p| item.build(p));

                fragment.append(built.js());

                built
            })
            .collect();


        p.put(ListProduct {
            list,
            fragment,
        })
    }

    fn update(self, p: &mut Self::Product) {
        let mut new = self.0.into_iter();
        let mut updated = 0;

        for (old, new) in p.list.iter_mut().zip(&mut new) {
            new.update(unsafe { old.as_mut().get_unchecked_mut() });
            updated += 1;
        }

        if p.list.len() > updated {
            for old in p.list[updated..].iter() {
                old.unmount();
            }
            p.list.truncate(updated);
        } else {
            for (old, new) in p.list[updated..].iter_mut().zip(&mut new) {
                new.update(unsafe { old.as_mut().get_unchecked_mut() });

                p.fragment.append(old.js());
            }

            for new in new {
                let built = In::boxed(|p| new.build(p));

                p.fragment.append(built.js());
                p.list.push(built);
            }
        }
    }
}

impl<V: View> View for Vec<V> {
    type Product = ListProduct<V::Product>;

    fn build(self, p: In<Self::Product>) -> Out<Self::Product> {
        List(self).build(p)
    }

    fn update(self, p: &mut Self::Product) {
        List(self).update(p);
    }
}

impl<'a, V> View for &'a [V]
where
    &'a V: View,
{
    type Product = ListProduct<<&'a V as View>::Product>;

    fn build(self, p: In<Self::Product>) -> Out<Self::Product> {
        List(self).build(p)
    }

    fn update(self, p: &mut Self::Product) {
        List(self).update(p)
    }
}

impl<V: View, const N: usize> View for [V; N] {
    type Product = ListProduct<V::Product>;

    fn build(self, p: In<Self::Product>) -> Out<Self::Product> {
        List(self).build(p)
    }

    fn update(self, p: &mut Self::Product) {
        List(self).update(p)
    }
}
