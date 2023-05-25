// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::diff::Diff;
use std::ops::{Deref, DerefMut};

pub struct VString {
    inner: String,
    ver: usize,
}

impl VString {
    pub fn new() -> VString {
        VString {
            inner: String::new(),
            ver: 0,
        }
    }

    pub fn with_capacity(capacity: usize) -> VString {
        VString {
            inner: String::with_capacity(capacity),
            ver: 0,
        }
    }

    pub fn into_inner(self) -> String {
        self.inner
    }
}

impl Diff for &'_ VString {
    // Compiler should be able to optimize this diff to
    // 64bit int diff on wasm32 targets
    type Memo = [usize; 2];

    fn into_memo(self) -> Self::Memo {
        [self.inner.as_ptr() as usize, self.ver]
    }

    fn diff(self, memo: &mut Self::Memo) -> bool {
        let m = self.into_memo();

        if *memo != m {
            *memo = m;
            true
        } else {
            false
        }
    }
}

impl Deref for VString {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for VString {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.ver += 1;

        &mut self.inner
    }
}

impl<S> From<S> for VString
where
    String: From<S>,
{
    fn from(value: S) -> Self {
        VString {
            inner: String::from(value),
            ver: 0,
        }
    }
}

impl Into<String> for VString {
    fn into(self) -> String {
        self.inner
    }
}

impl<S> PartialEq<S> for VString
where
    String: PartialEq<S>,
{
    fn eq(&self, other: &S) -> bool {
        self.inner.eq(other)
    }
}

impl<A> FromIterator<A> for VString
where
    String: FromIterator<A>,
{
    fn from_iter<T>(iter: T) -> VString
    where
        T: IntoIterator<Item = A>,
    {
        VString {
            inner: String::from_iter(iter),
            ver: 0,
        }
    }
}
