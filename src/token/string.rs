use std::{
    cmp::Ordering,
    fmt::{self, Write},
    ops::{Deref, DerefMut},
};

use super::{EscapeArgs, Token};

/// A string of tokens, similar to the [`str`] or [`[char]`](slice) types. It is usually seen in
/// borrowed form, `&TokenStr`.
#[derive(Eq, Hash, PartialEq)]
#[repr(transparent)]
pub struct TokenStr([Token]);

impl fmt::Debug for TokenStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_char('\"')?;
        for t in &self.0 {
            fmt::Display::fmt(
                &t.escape(EscapeArgs {
                    escape_single_quote: false,
                    ..EscapeArgs::ALL
                }),
                f,
            )?;
        }
        f.write_char('\"')
    }
}

impl fmt::Display for TokenStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for t in &self.0 {
            fmt::Display::fmt(t, f)?;
        }
        Ok(())
    }
}

impl AsRef<[Token]> for TokenStr {
    fn as_ref(&self) -> &[Token] {
        &self.0
    }
}

impl AsMut<[Token]> for TokenStr {
    fn as_mut(&mut self) -> &mut [Token] {
        &mut self.0
    }
}

impl Deref for TokenStr {
    type Target = [Token];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for TokenStr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Ordered by length, and then lexically.
impl Ord for TokenStr {
    fn cmp(&self, other: &Self) -> Ordering {
        self.len()
            .cmp(&other.len())
            .then_with(|| self.0.cmp(&other.0))
    }
}

/// Ordered by length, and then lexically.
impl PartialOrd for TokenStr {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a> IntoIterator for &'a TokenStr {
    type IntoIter = std::slice::Iter<'a, Token>;

    type Item = &'a Token;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

/// An owned string of tokens, similar to the [`String`] or [`Vec<char>`] types.
#[derive(Clone, Default, Eq, Hash, PartialEq)]
#[repr(transparent)]
pub struct TokenString(Vec<Token>);

impl TokenString {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn push(&mut self, token: Token) {
        self.0.push(token)
    }

    pub fn clear(&mut self) {
        self.0.clear()
    }

    pub fn extend_from_slice(&mut self, other: &TokenStr) {
        self.0.extend_from_slice(other)
    }
}

impl fmt::Debug for TokenString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&**self, f)
    }
}

impl fmt::Display for TokenString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&**self, f)
    }
}

impl Deref for TokenString {
    type Target = TokenStr;

    fn deref(&self) -> &Self::Target {
        // SAFETY: `TokenStr` is a `repr(transparent)` wrapper around `[Token]`
        unsafe { std::mem::transmute::<&[Token], &TokenStr>(&*self.0) }
    }
}

impl DerefMut for TokenString {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // SAFETY: `TokenStr` is a `repr(transparent)` wrapper around `[Token]`
        unsafe { std::mem::transmute::<&mut [Token], &mut TokenStr>(&mut *self.0) }
    }
}

/// Ordered by length, and then lexically.
impl Ord for TokenString {
    fn cmp(&self, other: &Self) -> Ordering {
        self.deref().cmp(other.deref())
    }
}

/// Ordered by length, and then lexically.
impl PartialOrd for TokenString {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl FromIterator<Token> for TokenString {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Token>,
    {
        TokenString(Vec::from_iter(iter))
    }
}

impl IntoIterator for TokenString {
    type IntoIter = std::vec::IntoIter<Token>;

    type Item = Token;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl From<Vec<Token>> for TokenString {
    fn from(vec: Vec<Token>) -> Self {
        Self(vec)
    }
}
