use std::cell::{Cell, UnsafeCell};

use wasm_bindgen::prelude::*;
use web_sys::Node;

use crate::dom::Element;
use crate::Html;

pub struct Static<F>(pub F);

impl<F> Html for Static<F>
where
    F: Fn() -> Node,
{
    type Product = Element;

    fn build(self) -> Element {
        Element::new(self.0())
    }

    fn update(self, _: &mut Element) {}
}

pub(crate) struct WithCell<T> {
    borrowed: Cell<bool>,
    data: UnsafeCell<T>,
}

impl<T> WithCell<T> {
    pub(crate) fn new(data: T) -> Self {
        WithCell {
            borrowed: Cell::new(false),
            data: UnsafeCell::new(data),
        }
    }

    pub(crate) fn with<F>(&self, mutator: F)
    where
        F: FnOnce(&mut T),
    {
        if self.borrowed.get() {
            return;
        }

        self.borrowed.set(true);
        mutator(unsafe { &mut *self.data.get() });
        self.borrowed.set(false);
    }
}

#[wasm_bindgen(module = "/js/util.js")]
extern "C" {
    pub(crate) fn __kobold_start(node: &JsValue);

    pub(crate) fn __kobold_append(parent: &Node, child: &JsValue);
    pub(crate) fn __kobold_before(node: &Node, insert: &JsValue);
    pub(crate) fn __kobold_unmount(node: &JsValue);
    pub(crate) fn __kobold_replace(old: &JsValue, new: &JsValue);

    pub(crate) fn __kobold_empty_node() -> Node;
    pub(crate) fn __kobold_fragment() -> Node;
    pub(crate) fn __kobold_fragment_decorate(f: &Node) -> Node;
    pub(crate) fn __kobold_fragment_append(f: &Node, c: &JsValue);
    pub(crate) fn __kobold_fragment_unmount(f: &Node);
    pub(crate) fn __kobold_fragment_replace(f: &Node, new: &JsValue);
    pub(crate) fn __kobold_fragment_drop(f: &Node);

    pub(crate) fn __kobold_text_node(t: &str) -> Node;

    pub(crate) fn __kobold_update_text(node: &Node, t: &str);

    pub(crate) fn __kobold_attr(name: &str, value: &str) -> Node;
    pub(crate) fn __kobold_attr_class(value: &str) -> Node;
    pub(crate) fn __kobold_attr_style(value: &str) -> Node;
    pub(crate) fn __kobold_attr_set(node: &JsValue, name: &str, value: &str) -> Node;
    pub(crate) fn __kobold_attr_update(node: &Node, value: &str);

    pub(crate) fn __kobold_attr_checked_set(el: &JsValue, value: bool);
    pub(crate) fn __kobold_class_set(el: &JsValue, value: &str);
    pub(crate) fn __kobold_class_add(el: &JsValue, value: &str);
    pub(crate) fn __kobold_class_remove(el: &JsValue, value: &str);
    pub(crate) fn __kobold_class_replace(el: &JsValue, old: &str, value: &str);
}
