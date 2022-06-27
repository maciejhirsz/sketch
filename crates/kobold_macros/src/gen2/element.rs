use std::fmt::{Display, Write};

use crate::dom2::{Attribute, AttributeValue, CssValue, HtmlElement};
use crate::gen2::{append, DomNode, Generator, IntoGenerator, Short};
use crate::parse::{IdentExt, Parse, TokenStreamExt};
use crate::syntax::InlineBind;

pub struct JsElement {
    /// Tag name of the element such as `div`
    pub tag: String,

    /// Variable name of the element, such as `e0`
    pub var: Short,

    /// Method calls on constructed element, such as `e0.append(foo);` or `e0.className = bar;`
    pub code: String,

    /// Arguments to import from rust
    pub args: Vec<Short>,
}

fn into_class_name<'a>(
    class: &'a mut Option<CssValue>,
    args: &mut Vec<Short>,
    gen: &'a mut Generator,
) -> Option<&'a dyn Display> {
    if let Some(CssValue::Literal(lit)) = class {
        return Some(lit);
    }

    if let Some(CssValue::Expression(expr)) = class.take() {
        let name = gen.add_expression(expr.stream);

        args.push(*name);

        return Some(name);
    }

    None
}

impl IntoGenerator for HtmlElement {
    fn into_gen(self, gen: &mut Generator) -> DomNode {
        let tag = self.name.to_string();
        let var = gen.out.next_el();
        let count = self.classes.len();

        let mut code = String::new();
        let mut args = Vec::new();

        let mut classes = self.classes.into_iter();

        if count == 1 {
            if let Some(class) = into_class_name(&mut classes.next(), &mut args, gen) {
                let _ = writeln!(code, "{var}.className={class};");
            }
        } else if let Some(first) = into_class_name(&mut classes.next(), &mut args, gen) {
            let _ = write!(code, "{var}.classList.add({first}");

            while let Some(class) = into_class_name(&mut classes.next(), &mut args, gen) {
                let _ = write!(code, ",{class}");
            }

            code.push_str(");\n");
        }

        for Attribute { name, value } in self.attributes {
            match value {
                AttributeValue::Literal(value) => {
                    let _ = writeln!(code, "{var}.setAttribute('{name}',{value});");
                }
                AttributeValue::Boolean(value) => {
                    let _ = writeln!(code, "{var}.{name}={value};");
                }
                AttributeValue::Expression(expr) => name.with_str(|name| {
                    if name.starts_with("on") && name.len() > 2 {
                        let target = match tag.as_str() {
                            "a" => "HtmlLinkElement",
                            "form" => "HtmlFormElement",
                            "img" => "HtmlImageElement",
                            "input" => "HtmlInputElement",
                            "option" => "HtmlOptionElement",
                            "select" => "HtmlSelectElement",
                            "textarea" => "HtmlTextAreaElement",
                            _ => "HtmlElement",
                        };

                        let mut inner = expr.stream.clone().parse_stream();

                        let callback = if let Ok(bind) = InlineBind::parse(&mut inner) {
                            let mut expr = bind.invocation;
                            write!(expr, "::<::kobold::reexport::web_sys::{target}, _, _> =");
                            expr.push(bind.arg);
                            expr
                        } else {
                            use proc_macro::{Delimiter, TokenStream};

                            let mut constrain = TokenStream::new();

                            write!(
                                constrain,
                                "let constrained: ::kobold:stateful::Callback<\
                                    ::kobold::reexport::web_sys::{target},\
                                    _,\
                                    _,\
                                > ="
                            );
                            constrain.extend(expr.stream);
                            constrain.write("; constrained");
                            constrain.group(Delimiter::Brace)
                        };

                        let event = &name[2..];
                        let value = gen.add_expression(callback);

                        args.push(*value);

                        let _ = writeln!(code, "{var}.addEventListener('{event}',{value});");

                        return;
                    }

                    let value = gen.add_expression(expr.stream);

                    args.push(*value);

                    let _ = writeln!(code, "{var}.setAttributeNode('{name}',{value});");
                }),
            };
        }

        if let Some(children) = self.children {
            let append = append(gen, &mut code, &mut args, children);
            let _ = writeln!(code, "{var}.{append}");
        }

        DomNode::Element(JsElement {
            tag,
            var,
            code,
            args,
        })
    }
}
