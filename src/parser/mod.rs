use std::error;
use std::fmt;

pub mod environment;
mod re;
pub mod regex;
pub mod tokenizer;

/// An error encountered during parsing.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Error {
    /// An unexpected '|' token.
    Or,
    /// An unexpected '&' token.
    And,
    /// An unexpected '!' token.
    Not,
    /// An unexpected '?' token.
    Question,
    /// An unexpected '*' token.
    Star,
    /// An unexpected '+' token.
    Plus,
    /// An unexpected '(' token.
    OpenParen,
    /// An unexpected ')' token.
    CloseParen,
    /// An unexpected literal token.
    Other,
    /// Unexpected end of input stream.
    EndOfInput,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", error::Error::description(self))
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        use self::Error::*;
        match *self {
            Or => "Unexpected '|'",
            And => "Unexpected '&'",
            Not => "Unexpected '!'",
            Question => "Unexpected '?'",
            Star => "Unexpected '*'",
            Plus => "Unexpected '+'",
            Other => "Unexpected token",
            CloseParen => "Unexpected ')'",
            OpenParen => "Unexpected '('",
            EndOfInput => "Unexpected end of input",
        }
    }
}
