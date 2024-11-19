// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::fmt::{self, Debug, Display, Write};

use arrayvec::ArrayString;
use tokens::{Ident, Literal, TokenStream};

use crate::gen::element::{Attr, InlineAbi};
use crate::gen::Short;
use crate::itertools::IteratorExt;
use crate::parse::IdentExt;
use crate::tokenize::prelude::*;

// JS function name, capacity must fit a `Short`, a hash, and few underscores
pub type JsFnName = ArrayString<24>;

#[derive(Default, Debug)]
pub struct Transient {
    pub js: JsModule,
    pub hints: Vec<Hint>,
    pub fields: Vec<Field>,
    pub els: Vec<Short>,
}

#[derive(Debug)]
pub struct Hint {
    pub name: Ident,
    pub typ: TokenStream,
}

impl Transient {
    fn is_const(&self) -> bool {
        let jsfn = match self.js.functions.first() {
            Some(fun) => fun,
            None => return false,
        };

        self.fields.is_empty()
            && self.els.len() == 1
            && self.js.functions.len() == 1
            && !matches!(jsfn.anchor, Anchor::Fragment)
            && jsfn.args.is_empty()
    }

    fn tokenize_const(self, stream: &mut TokenStream) {
        let JsFunction { name, .. } = self.js.functions[0];

        block((
            "use ::kobold::reexport::wasm_bindgen;",
            self.js,
            format_args!("::kobold::internal::Precompiled({name})"),
        ))
        .tokenize_in(stream)
    }

    fn transient_signature(&self) -> TokenStream {
        let mut generics = String::new();
        let mut declare = String::new();

        for field in self.fields.iter() {
            let typ = field.make_type();

            field.declare(&mut declare);

            let _ = write!(generics, "{typ},");
        }

        let mut bounds = "where".tokenize();

        for field in self.fields.iter() {
            field.bounds(&mut bounds);
        }

        (
            format_args!("struct Transient <{generics}>"),
            bounds.clone(),
            block(declare.as_str()),
            format_args!("impl<{generics}> ::kobold::View for Transient<{generics}>"),
            bounds,
        )
            .tokenize()
    }

    fn type_hints(&mut self) -> TokenStream {
        if self.hints.is_empty() {
            return TokenStream::new();
        }

        let mut stream = TokenStream::new();

        for (i, hint) in self.hints.drain(..).enumerate() {
            let name = hint.name.with_str(|h| Ident::new_raw(h, hint.name.span()));

            stream.write((
                call(format_args!("fn _hint_{i}"), (name, ':', hint.typ)),
                block(()),
            ))
        }

        ("#[allow(unused_variables)]", block(stream)).tokenize()
    }
}

impl Tokenize for Transient {
    fn tokenize_in(mut self, stream: &mut TokenStream) {
        if self.is_const() {
            self.tokenize_const(stream);
            return;
        }

        let transient_signature = self.transient_signature();
        let attr_hints = self.type_hints();

        if self.els.is_empty() {
            return self.fields.remove(0).value.tokenize_in(stream);
        }

        let mut generics = String::new();

        // let mut prebuild = String::new();
        let mut build = String::new();
        let mut build2 = String::new();
        let mut build_fields = String::new();
        let mut update = String::new();
        let mut declare = String::new();

        let mut trigger = String::new();
        let mut trigger_bounds = String::new();

        let mut product_declare = String::new();
        let mut product_generics = String::new();
        let mut product_generics_bounds = String::new();

        let events = self
            .fields
            .iter()
            .filter(|field| matches!(field.kind, FieldKind::Event { .. }))
            .count();

        if events > 0 {
            let _ = write!(
                build,
                "let eid_root = ::kobold::runtime::EventId::reserve({events});"
            );

            product_declare.push_str("eid_root: ::kobold::runtime::EventId,");
            build_fields.push_str("eid_root,");
        }

        let mut event = 0;

        for field in self.fields.iter() {
            let typ = field.make_type();

            let _ = write!(generics, "{typ},");

            field.build(&mut build, &mut build2, event);
            field.update(&mut update);
            field.declare(&mut declare);
            field.trigger(&mut trigger, &mut event);

            match field.kind {
                FieldKind::StaticView => (),
                FieldKind::Event { event, target } => {
                    let _ = write!(product_generics, "{typ},");
                    let _ = write!(product_generics_bounds, "{typ},");
                    let _ = write!(build_fields, "{}: self.{},", field.name, field.name);
                    let _ = write!(trigger_bounds,
                        "{typ}: ::kobold::event::Listener<\
                            ::kobold::event::{event}<\
                                ::kobold::reexport::web_sys::{target}\
                            >
                        >,"
                    );
                    field.declare(&mut product_declare);
                }
                _ => {
                    let _ = write!(product_generics, "{typ},");
                    let _ = write!(product_generics_bounds, "{typ}::Product,");
                    let _ = write!(build_fields, "{},", field.name);
                    field.declare(&mut product_declare);
                }
            }

            if let FieldKind::View { .. } = field.kind {
                let _ = write!(trigger_bounds, "{typ}: ::kobold::runtime::Trigger,");
            }
        }

        let mut declare_els = String::new();

        for (jsfn, el) in self.js.functions.iter().zip(self.els) {
            let JsFunction { name, anchor, args } = jsfn;

            let anchor_type = anchor.as_type();

            let _ = write!(declare_els, "{el}: {anchor_type},");

            let args = args
                .iter()
                .map(|a| {
                    let mut temp = ArrayString::<24>::new();

                    let _ = write!(temp, "{a}");

                    temp
                })
                .join(",");

            let _ = write!(build, "let {el} = {anchor_type}::from({name}({args}));");
            let _ = write!(build_fields, "{el},");
        }
        let anchor = &self.js.functions.last().unwrap().anchor;

        let anchor_type = anchor.as_type();
        let anchor_js_type = anchor.as_js_type();

        block((
            (
                "\
                use ::kobold::dom::Mountable as _;\
                use ::kobold::reexport::wasm_bindgen;\
                ",
                self.js,
                format_args!(
                    "\
                    struct TransientProduct <{product_generics}> {{\
                        {product_declare}\
                        {declare_els}\
                    }}\
                    \
                    impl<{product_generics}> ::kobold::runtime::Trigger for TransientProduct<{product_generics}> \
                    where \
                        {trigger_bounds}\
                    {{\
                        fn trigger<Ctx>(&mut self, ctx: &mut Ctx) -> Option<::kobold::runtime::Then> \
                        where \
                            Ctx: ::kobold::runtime::EventContext,\
                        {{\
                            {trigger}
                            None\
                        }}\
                    \
                    }}\
                    \
                    impl<{product_generics}> ::kobold::dom::Anchor for TransientProduct<{product_generics}> \
                    where \
                        Self: 'static,\
                    ",
                ),
            ),
            block((
                "type Js = ::kobold::reexport::web_sys::",
                anchor_js_type,
                ";",
                format_args!("\
                    type Target = {anchor_type};

                    fn anchor(&self) -> &Self::Target {{\
                        &self.e0\
                    }}\
                "),
            )),
            transient_signature,
            format_args!(
                "\
                {{\
                    type Product = TransientProduct<{product_generics_bounds}>;\
                    \
                    fn build(self) -> Self::Product {{\
                        {build}\
                        {build2}\
                        TransientProduct {{\
                            {build_fields}\
                        }}
                    }}\
                    \
                    fn update(self, p: &mut Self::Product) {{\
                        {update}\
                    }}\
                }}\
                \
                "
            ),
            (
                attr_hints,
                "Transient",
                block(each(self.fields.iter().map(Field::invoke))),
            ),
        ))
        .tokenize_in(stream)
    }
}

#[derive(Default)]
pub struct JsModule {
    pub functions: Vec<JsFunction>,
    pub code: String,
}

impl Tokenize for JsModule {
    fn tokenize_in(self, stream: &mut TokenStream) {
        if self.functions.is_empty() {
            return;
        }

        stream.write((
            '#',
            group(
                '[',
                (
                    "wasm_bindgen::prelude::wasm_bindgen",
                    group('(', ("inline_js = ", string(&self.code))),
                ),
            ),
            "extern \"C\"",
            block(each(self.functions)),
        ))
    }
}

impl Debug for JsModule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Debug::fmt(&self.functions, f)?;

        write!(f, "\ncode: ({})", self.code)
    }
}

pub struct JsAttrConstructor(pub JsFnName);

impl Tokenize for JsAttrConstructor {
    fn tokenize_in(self, stream: &mut TokenStream) {
        let name = self.0;
        stream.write(format_args!(
            "fn {name}() -> ::kobold::reexport::web_sys::Node;"
        ));
    }
}

#[derive(Debug)]
pub struct JsFunction {
    pub name: JsFnName,
    pub anchor: Anchor,
    pub args: Vec<JsArgument>,
}

#[derive(Debug, Clone, Copy)]
pub enum Anchor {
    Element(&'static str),
    Node,
    Fragment,
}

impl Anchor {
    fn as_js_type(&self) -> &'static str {
        match self {
            Anchor::Element(typ) => typ,
            Anchor::Node | Anchor::Fragment => "Node",
        }
    }

    fn as_type(&self) -> &'static str {
        match self {
            Anchor::Element(_) | Anchor::Node => "::kobold::reexport::web_sys::Node",
            Anchor::Fragment => "::kobold::dom::Fragment",
        }
    }
}

impl Tokenize for JsFunction {
    fn tokenize_in(self, stream: &mut TokenStream) {
        let name = self.name;

        stream.write((
            call(format_args!("fn {name}"), each(self.args)),
            "-> ::kobold::reexport::web_sys::Node;",
        ));
    }
}

#[derive(Debug)]
pub struct JsArgument {
    pub name: Short,
    pub abi: Option<InlineAbi>,
}

impl JsArgument {
    pub fn new(name: Short) -> Self {
        JsArgument { name, abi: None }
    }

    pub fn with_abi(name: Short, abi: InlineAbi) -> Self {
        JsArgument {
            name,
            abi: Some(abi),
        }
    }
}

impl Display for JsArgument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = self.name;

        match self.abi {
            Some(InlineAbi::Bool) => write!(f, "self.{name}.into()"),
            Some(InlineAbi::Str) => write!(f, "self.{name}.as_ref()"),
            Some(InlineAbi::Event) => write!(f, "{name}.js_handler()"),
            None => write!(f, "{name}.js()"),
        }
    }
}

impl Tokenize for JsArgument {
    fn tokenize_in(self, stream: &mut TokenStream) {
        let abi = self
            .abi
            .map(InlineAbi::abi)
            .unwrap_or("&wasm_bindgen::JsValue");

        (ident(&self.name), ':', abi, ',').tokenize_in(stream)
    }
}

pub struct Field {
    pub name: Short,
    pub value: TokenStream,
    pub kind: FieldKind,
}

pub enum FieldKind {
    StaticView,
    View,
    Event {
        event: &'static str,
        target: &'static str,
    },
    Attribute {
        el: Short,
        attr: Attr,
        prop: TokenStream,
    },
}

impl Debug for Field {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Field { name, value, kind } = self;

        match kind {
            FieldKind::StaticView => {
                write!(f, "{name} <StaticView>: {value}")
            }
            FieldKind::View => {
                write!(f, "{name} <View>: {value}")
            }
            FieldKind::Event { event, target } => {
                write!(f, "{name} <Listener<{event}<{target}>>>: {value}")
            }
            FieldKind::Attribute { attr, .. } => {
                write!(f, "{name} <Attribute<{}>>: {value}", attr.name)
            }
        }
    }
}

impl Field {
    pub fn new(name: Short, value: TokenStream) -> Self {
        Field {
            name,
            value,
            kind: FieldKind::View,
        }
    }

    pub fn event(&mut self, event: &'static str, target: &'static str) -> &mut Self {
        self.kind = FieldKind::Event { event, target };
        self
    }

    pub fn attr(&mut self, el: Short, attr: Attr, prop: TokenStream) -> &mut Self {
        self.kind = FieldKind::Attribute { el, attr, prop };
        self
    }

    fn name_value(&self) -> (&Short, &TokenStream) {
        (&self.name, &self.value)
    }

    fn make_type(&self) -> Short {
        let (name, _) = self.name_value();

        let mut typ = *name;
        typ.make_ascii_uppercase();
        typ
    }

    fn bounds(&self, buf: &mut TokenStream) {
        let Field { name, kind, .. } = self;

        let mut typ = *name;
        typ.make_ascii_uppercase();

        match kind {
            FieldKind::View | FieldKind::StaticView => {
                buf.write((typ.as_str(), ": ::kobold::View,"));
            }
            FieldKind::Event { event, target } => {
                buf.write(format_args!(
                    "{typ}: ::kobold::event::Listener<\
                        ::kobold::event::{event}<\
                            ::kobold::reexport::web_sys::{target}\
                        >
                    >,"
                ));
            }
            FieldKind::Attribute { attr, .. } => {
                let (amp, attr_name) = attr.as_parts();
                buf.write((
                    format_args!(
                        "{typ}: ::kobold::attribute::Attribute<{amp}::kobold::attribute::{attr_name}>"
                    ),
                    attr.abi.map(InlineAbi::bound),
                    ',',
                ));
            }
        }
    }

    fn declare(&self, buf: &mut String) {
        let (name, _) = self.name_value();
        let typ = self.make_type();

        let _ = write!(buf, "{name}: {typ},");
    }

    fn build(&self, build: &mut String, build2: &mut String, event: usize) {
        let Field { name, kind, .. } = self;

        match kind {
            FieldKind::StaticView => {
                let _ = write!(
                    build,
                    "\
                    let {name} = self.{name}.build();\
                    "
                );
            }
            FieldKind::View => {
                let _ = write!(build, "let {name} = self.{name}.build();");
            }
            FieldKind::Event { .. } => {
                let _ = write!(build, "let {name} = eid_root.offset({event});");
            }
            FieldKind::Attribute { attr, .. } if attr.abi.is_some() => {
                let _ = write!(build2, "let {name} = self.{name}.build();");
            }
            FieldKind::Attribute { el, prop, .. } => {
                let _ = write!(build2, "let {name} = self.{name}.build_in({prop}, &{el});");
            }
        }
    }

    fn trigger(&self, buf: &mut String, event: &mut usize) {
        let Field { name, kind, .. } = self;

        match kind {
            FieldKind::StaticView | FieldKind::Attribute { .. } => (),
            FieldKind::View => {
                let _ = write!(
                    buf,
                    "if let Some(then) = self.{name}.trigger(ctx) {{ return Some(then) }}"
                );
            }
            FieldKind::Event { .. } => {
                let _ = write!(
                    buf,
                    "if let Some(then) = self.{name}.trigger(ctx, self.eid_root.offset({event})) {{ return Some(then) }}"
                );
                *event += 1;
            }
        }
    }

    fn update(&self, buf: &mut String) {
        let Field { name, kind, .. } = self;

        match kind {
            FieldKind::StaticView => (),
            FieldKind::View | FieldKind::Event { .. } => {
                let _ = write!(buf, "self.{name}.update(&mut p.{name});");
            }
            FieldKind::Attribute { el, prop, .. } => {
                let _ = write!(
                    buf,
                    "self.{name}.update_in({prop}, &p.{el}, &mut p.{name});"
                );
            }
        }
    }

    fn invoke(&self) -> impl Tokenize {
        let (name, value) = self.name_value();

        (ident(name), ':', value.clone(), ',')
    }
}

pub struct JsString(pub Literal);

impl Display for JsString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let stringified = self.0.to_string();

        match stringified.chars().next() {
            // Take the string verbatim
            Some('"' | '\'') => f.write_str(&stringified),
            // Add quotes
            _ => write!(f, "\"{stringified}\""),
        }
    }
}
