//! Keyword handles for `{ ... }` expressions in the [`view!`](crate::view) macro.

use crate::diff::{NoDiff, RefDiff};
use crate::list::List;
use crate::View;

/// `{ for ... }`: turn an [`IntoIterator`](IntoIterator) type into a [`View`](View).
pub const fn r#for<T>(iterator: T) -> List<T>
where
    T: IntoIterator,
    T::Item: View,
{
    List(iterator)
}

/// `{ ref ... }`: diff this value by its reference address.
///
/// For strings this is both faster and more memory efficient (no allocations necessary),
/// however it might fail to update if underlying memory has been mutated in place without
/// re-allocations.
pub const fn r#ref(value: &str) -> RefDiff<str> {
    RefDiff(value)
}

/// `{ override ... }`: disable diffing for `T` and apply its value to the DOM on every render.
pub const fn r#override<T>(value: T) -> AlwaysUpdate<T> {
    NoDiff(value)
}

/// `{ static ... }` disable diffing for `T` and never update its value in the DOM after the initial render.
pub const fn r#static<T>(value: T) -> NeverUpdate<T> {
    NoDiff(value)
}

pub type NeverUpdate<T> = NoDiff<T, false>;

pub type AlwaysUpdate<T> = NoDiff<T, true>;
