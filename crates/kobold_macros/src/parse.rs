//! [`ParseStream`](ParseStream), the [`Parse`](Parse) trait and utilities for working with
//! token streams without `syn` or `quote`.

use beef::Cow;
use proc_macro::{Delimiter, Ident, Spacing, Span, TokenStream, TokenTree};

pub type ParseStream = std::iter::Peekable<proc_macro::token_stream::IntoIter>;

pub mod prelude {
    pub use super::{
        IteratorExt, Lit, Parse, ParseError, ParseStream, TokenStreamExt, TokenTreeExt,
    };
}

#[derive(Debug)]
pub struct ParseError {
    pub msg: Cow<'static, str>,
    pub tt: Option<TokenTree>,
}

impl ParseError {
    pub fn new<S: Into<Cow<'static, str>>>(msg: S, tt: Option<TokenTree>) -> Self {
        let mut error = ParseError::from(tt);

        error.msg = msg.into();
        error
    }

    pub fn tokenize(self) -> TokenStream {
        let msg = self.msg.as_ref();
        let span = self
            .tt
            .as_ref()
            .map(|tt| tt.span())
            .unwrap_or_else(Span::call_site)
            .into();

        (quote::quote_spanned! { span =>
            fn _parse_error() {
                compile_error!(#msg)
            }
        })
        .into()
    }
}

impl From<Option<TokenTree>> for ParseError {
    fn from(tt: Option<TokenTree>) -> Self {
        ParseError {
            msg: "Unexpected token".into(),
            tt,
        }
    }
}

pub trait Pattern: Copy {
    fn matches(self, tt: &TokenTree) -> bool;

    fn expected(self) -> Cow<'static, str>;
}

pub trait Parse: Sized {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError>;
}

impl Parse for Ident {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        match stream.next() {
            Some(TokenTree::Ident(ident)) => Ok(ident),
            tt => Err(ParseError::new("Expected an identifier", tt)),
        }
    }
}

impl Parse for () {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        match stream.next() {
            tt @ Some(_) => Err(ParseError::new("Unexpected token", tt)),
            _ => Ok(()),
        }
    }
}

impl<T: Parse> Parse for Vec<T> {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let mut items = Vec::new();

        while !stream.end() {
            items.push(stream.parse()?);
        }

        Ok(items)
    }
}

#[derive(Clone, Copy)]
pub struct Lit;

impl Pattern for Lit {
    fn matches(self, tt: &TokenTree) -> bool {
        match tt {
            TokenTree::Literal(_) => true,
            _ => false,
        }
    }

    fn expected(self) -> Cow<'static, str> {
        "Expected a literal value".into()
    }
}

impl Pattern for &str {
    fn matches(self, tt: &TokenTree) -> bool {
        match tt {
            TokenTree::Ident(ident) => ident.to_string() == self,
            _ => false,
        }
    }

    fn expected(self) -> Cow<'static, str> {
        format!("Expected {self}").into()
    }
}

impl Pattern for char {
    fn matches(self, tt: &TokenTree) -> bool {
        match tt {
            TokenTree::Punct(punct) => punct.as_char() == self,
            _ => false,
        }
    }

    fn expected(self) -> Cow<'static, str> {
        format!("Expected {self}").into()
    }
}

impl Pattern for (char, Spacing) {
    fn matches(self, tt: &TokenTree) -> bool {
        match tt {
            TokenTree::Punct(punct) => punct.as_char() == self.0 && punct.spacing() == self.1,
            _ => false,
        }
    }

    fn expected(self) -> Cow<'static, str> {
        format!("Expected {}", self.0).into()
    }
}

impl Pattern for Delimiter {
    fn matches(self, tt: &TokenTree) -> bool {
        match tt {
            TokenTree::Group(group) => group.delimiter() == self,
            _ => false,
        }
    }

    fn expected(self) -> Cow<'static, str> {
        match self {
            Delimiter::Parenthesis => "Expected (...)",
            Delimiter::Brace => "Expected {...}",
            Delimiter::Bracket => "Expected [...]",
            Delimiter::None => "Expected a group",
        }
        .into()
    }
}

pub trait IteratorExt {
    fn expect(&mut self, pattern: impl Pattern) -> Result<TokenTree, ParseError>;

    fn allow(&mut self, pattern: impl Pattern) -> bool;

    fn allow_consume(&mut self, pattern: impl Pattern) -> Option<TokenTree>;

    fn parse<T: Parse>(&mut self) -> Result<T, ParseError>;

    fn end(&mut self) -> bool;
}

impl IteratorExt for ParseStream {
    fn expect(&mut self, pattern: impl Pattern) -> Result<TokenTree, ParseError> {
        match self.next() {
            Some(tt) if pattern.matches(&tt) => Ok(tt),
            tt => Err(ParseError::new(pattern.expected(), tt)),
        }
    }

    fn allow(&mut self, pattern: impl Pattern) -> bool {
        self.peek().map(|tt| pattern.matches(tt)).unwrap_or(false)
    }

    fn allow_consume(&mut self, pattern: impl Pattern) -> Option<TokenTree> {
        self.next_if(|tt| pattern.matches(tt))
    }

    fn parse<T: Parse>(&mut self) -> Result<T, ParseError> {
        T::parse(self)
    }

    fn end(&mut self) -> bool {
        self.peek().is_none()
    }
}

pub trait TokenTreeExt {
    fn is(&self, pattern: impl Pattern) -> bool;
}

impl TokenTreeExt for TokenTree {
    fn is(&self, pattern: impl Pattern) -> bool {
        pattern.matches(self)
    }
}

impl TokenTreeExt for Option<TokenTree> {
    fn is(&self, pattern: impl Pattern) -> bool {
        self.as_ref().map(|tt| pattern.matches(tt)).unwrap_or(false)
    }
}

pub trait TokenStreamExt {
    fn write(&mut self, rust: &str);

    fn push(&mut self, tt: impl Into<TokenTree>);

    fn parse_stream(self) -> ParseStream;
}

impl TokenStreamExt for TokenStream {
    fn write(&mut self, rust: &str) {
        use std::str::FromStr;

        self.extend(TokenStream::from_str(rust).unwrap());
    }

    fn push(&mut self, tt: impl Into<TokenTree>) {
        self.extend([tt.into()]);
    }

    fn parse_stream(self) -> ParseStream {
        self.into_iter().peekable()
    }
}
