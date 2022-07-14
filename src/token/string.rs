use std::{
    fmt::{self, Write},
    ops::{Deref, DerefMut},
};

use super::{EscapeArgs, Token};

/// A string of tokens, similar to the [`str`] or [`[char]`](slice) types. It is usually seen in
/// borrowed form, `&TokenStr`.
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

/// An owned string of tokens, similar to the [`String`] or [`Vec<char>`] types.
#[repr(transparent)]
pub struct TokenString(Vec<Token>);

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
