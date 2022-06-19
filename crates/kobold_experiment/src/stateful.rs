use std::cell::RefCell;
use std::marker::PhantomData;
use std::rc::{Rc, Weak};

use wasm_bindgen::closure::Closure;
use wasm_bindgen::JsValue;
use web_sys::Event;

use crate::{Html, Mountable, ShouldRender};

pub trait Stateful {
    type State: 'static;

    fn init(self) -> Self::State;

    fn update(self, state: &mut Self::State) -> ShouldRender;
}

impl<T: Copy + Eq + 'static> Stateful for T {
    type State = Self;

    fn init(self) -> Self::State {
        self
    }

    fn update(self, state: &mut Self::State) -> ShouldRender {
        if self != *state {
            *state = self;
            true
        } else {
            false
        }
    }
}

pub trait HasUpdated {
    fn has_updated(self) -> bool;
}

impl HasUpdated for () {
    fn has_updated(self) -> bool {
        true
    }
}

impl HasUpdated for bool {
    fn has_updated(self) -> bool {
        self
    }
}

pub struct WithState<S: Stateful, H: Html> {
    props: S,
    render: RenderFn<S::State, H::Product>,
    _marker: PhantomData<H>,
}

pub fn stateful<'a, S, H>(
    props: S,
    render: fn(&'a S::State, &'a Link<S::State, H::Product>) -> H,
) -> WithState<S, H>
where
    S: Stateful,
    H: Html + 'a,
{
    WithState {
        props,
        render: RenderFn::new(render),
        _marker: PhantomData,
    }
}

/// Magic wrapper for render function that allows us to store it with a 'static
/// lifetime, without the lifetime on return type getting in the way
struct RenderFn<S, P> {
    ptr: usize,
    _marker: PhantomData<(S, P)>,
}

impl<S, P> Clone for RenderFn<S, P> {
    fn clone(&self) -> Self {
        RenderFn {
            ptr: self.ptr,
            _marker: PhantomData,
        }
    }
}

impl<S, P> Copy for RenderFn<S, P> {}

impl<S, P> RenderFn<S, P> {
    fn new<'a, H>(render: fn(&'a S, &'a Link<S, P>) -> H) -> Self
    where
        H: Html<Product = P> + 'a,
    {
        RenderFn {
            ptr: render as usize,
            _marker: PhantomData,
        }
    }

    unsafe fn cast<'a, H>(self) -> fn(&'a S, &'a Link<S, P>) -> H
    where
        H: Html<Product = P> + 'a,
    {
        std::mem::transmute(self.ptr)
    }
}

struct Inner<S, P> {
    state: RefCell<S>,
    product: RefCell<P>,
    render: RenderFn<S, P>,
    link: Link<S, P>,
    update: fn(RenderFn<S, P>, &Link<S, P>),
}

impl<S, P> Inner<S, P> {
    fn update(&self) {
        (self.update)(self.render, &self.link)
    }
}

pub struct WithStateProduct<S, P> {
    inner: Rc<Inner<S, P>>,
    js: JsValue,
}

pub struct Link<S, P> {
    inner: Weak<Inner<S, P>>,
}

pub struct Callback<F, L> {
    cb: F,
    link: L,
}

// I should not need to write this, but lifetime checking
// was going really off the rails with inlined boxing
#[inline]
fn make_closure<F>(fun: F) -> Box<dyn FnMut(&Event)>
where
    F: FnMut(&Event) + 'static,
{
    Box::new(fun)
}

pub struct CallbackProduct {
    closure: Closure<dyn FnMut(&Event)>,
}

impl<F, A, S, P> Html for Callback<F, &Link<S, P>>
where
    F: Fn(&mut S) -> A + 'static,
    A: HasUpdated,
    S: 'static,
    P: 'static,
{
    type Product = CallbackProduct;

    fn build(self) -> Self::Product {
        let link = self.link.clone();
        let cb = self.cb;

        let closure = make_closure(move |_event| {
            if let Some(rc) = link.inner.upgrade() {
                if cb(&mut rc.state.borrow_mut()).has_updated() {
                    rc.update();
                }
            }
        });
        let closure = Closure::wrap(closure);

        CallbackProduct { closure }
    }

    fn update(self, _p: &mut Self::Product) {}
}

impl Mountable for CallbackProduct {
    fn js(&self) -> &JsValue {
        self.closure.as_ref()
    }
}

impl<S, P> Link<S, P>
where
    S: 'static,
    P: 'static,
{
    pub fn bind<F, A>(&self, cb: F) -> Callback<F, &Self>
    where
        F: Fn(&mut S) -> A + 'static,
        A: HasUpdated,
    {
        Callback { cb, link: self }
    }
}

impl<S, P> Clone for Link<S, P> {
    fn clone(&self) -> Self {
        Link {
            inner: self.inner.clone(),
        }
    }
}

impl<S, H> Html for WithState<S, H>
where
    S: Stateful,
    H: Html,
{
    type Product = WithStateProduct<S::State, H::Product>;

    fn build(self) -> Self::Product {
        let state = self.props.init();

        let inner = Rc::new_cyclic(move |inner| {
            let link = Link {
                inner: inner.clone(),
            };

            // Safety: this is safe as long as `S` and `H` are the same types that
            // were used to create this `RenderFn` instance.
            let render_fn = unsafe { self.render.cast::<H>() };
            let product = (render_fn)(&state, &link).build();

            Inner {
                state: RefCell::new(state),
                product: RefCell::new(product),
                render: self.render,
                link,
                update: |render, link| {
                    // Safety: this is safe as long as `S` and `H` are the same types that
                    // were used to create this `RenderFn` instance.
                    let render = unsafe { render.cast::<H>() };

                    if let Some(inner) = link.inner.upgrade() {
                        (render)(&inner.state.borrow(), &link)
                            .update(&mut inner.product.borrow_mut());
                    }
                },
            }
        });

        let js = inner.product.borrow().js().clone();

        WithStateProduct { inner, js }
    }

    fn update(self, p: &mut Self::Product) {
        if self.props.update(&mut p.inner.state.borrow_mut()) {
            p.inner.update();
        }
    }
}

impl<S, P> Mountable for WithStateProduct<S, P>
where
    S: 'static,
    P: Mountable,
{
    fn js(&self) -> &JsValue {
        &self.js
    }
}
