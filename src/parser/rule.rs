use std::iter::Peekable;
use std::error;
use std::fmt;

use token::Token;

struct Parser<I: Iterator>(Peekable<I>, usize);

impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    fn peek(&mut self) -> Option<&Token> {
        self.0.peek()
    }

    fn next(&mut self) -> Option<Token> {
        self.0.next().map(|t| {
            self.1 += 1;
            t
        })
    }

    /// Returns the number of tokens that have been read so far.
    fn index(&self) -> usize {
        self.1
    }

    /// Parses a category from the stream.
    ///
    /// This should be used _after_ the initial `'{'` has been popped.
    fn parse_category(&mut self) -> Result<Category, Error> {
        if let Some(&t) = self.peek() {
            if t.is_digit() {
                // get the number
                let number = Some(self.parse_number());
                match self.next() {
                    Some(Token::Colon) => self.finish_parse_category(number),
                    Some(tok) => Err(Error::Token(self.index() - 1, tok)),
                    None => Err(Error::EndOfInput),
                }
            } else {
                self.finish_parse_category(None)
            }
        } else {
            Err(Error::EndOfInput)
        }
    }

    /// Finishes parsing a category, after the number (or lack thereof) has been matched.
    fn finish_parse_category(&mut self, number: Option<u8>) -> Result<Category, Error> {
        let name = self.parse_ident();
        match self.next() {
            Some(Token::CloseBrace) => Ok(Category { name, number }),
            Some(tok) => Err(Error::Token(self.index() - 1, tok)),
            None => Err(Error::EndOfInput),
        }
    }

    /// Parses a number from the stream.
    ///
    /// # Panics
    /// Panics if the value excedes 255.
    fn parse_number(&mut self) -> u8 {
        let mut num = 0;
        // peek at the next token, and extract it's value if it's a digit, or exit the loop
        // otherwise
        while let Some(x) = self.peek().and_then(|&tok| tok.digit_value()) {
            num = num * 10 + x;
            self.next();
        }
        num
    }

    /// Parses an identifier from the stream.
    fn parse_ident(&mut self) -> Ident {
        let mut id = Ident::new();
        while !self.peek().map_or(false, |t| t.is_control_token()) {
            id.push(self.next().unwrap());
        }
        id
    }
}

pub type Ident = Vec<Token>;

pub struct Category {
    /// The name of the category.
    pub name: Ident,
    /// The slot to associate the category with.
    pub number: Option<u8>,
}

/// An error encountered during parsing.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Error {
    /// An unexpected token, and the index it occurred at.
    Token(usize, Token),
    /// Unexpected end of input.
    EndOfInput,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::Token(idx, tok) => write!(f, "Unexpected token `{:?}` at position {}", tok, idx),
            Error::EndOfInput => write!(f, "Unexpected end of input"),
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        use self::Error::*;
        match *self {
            Token(_, _) => "unexpected token",
            EndOfInput => "unexpected end of input",
        }
    }
}
