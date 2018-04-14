use std::iter::Peekable;
use std::error;
use std::fmt;

use ast::{Pattern, Repeater, Category, Ident};
use token::Token;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_number() {
        let input = vec![
            Token::try_from_u8(b'1').unwrap(),
            Token::try_from_u8(b'2').unwrap(),
            Token::try_from_u8(b'3').unwrap(),
        ];
        let mut parser = Parser(input.into_iter().peekable(), 0);
        let num = parser.parse_number();
        assert_eq!(num, 123);
        assert_eq!(parser.next(), None);
    }

    #[test]
    fn parse_ident() {
        let a = Token::try_from_u8(b'a').unwrap();
        let b = Token::try_from_u8(b'b').unwrap();
        let c = Token::try_from_u8(b'c').unwrap();
        let input = vec![a, b, c];
        let mut parser = Parser(input.into_iter().peekable(), 0);
        let ident = parser.parse_ident();
        assert_eq!(ident, vec![a, b, c]);
    }

    #[test]
    fn parse_category_no_number() {
        let c = Token::try_from_u8(b'C').unwrap();
        let input = vec![Token::OpenBrace, c, Token::CloseBrace];
        let mut parser = Parser(input.into_iter().peekable(), 0);
        let result = parser.parse_regex();
        assert_eq!(
            result,
            Ok(Pattern::Category(Category {
                name: vec![c],
                number: None,
            }))
        );
    }

    #[test]
    fn parse_category_number() {
        let c = Token::try_from_u8(b'C').unwrap();
        let input = vec![
            Token::OpenBrace,
            Token::try_from_u8(b'3').unwrap(),
            Token::Colon,
            c,
            Token::CloseBrace,
        ];
        let mut parser = Parser(input.into_iter().peekable(), 0);
        let result = parser.parse_regex();
        assert_eq!(
            result,
            Ok(Pattern::Category(Category {
                name: vec![c],
                number: Some(3),
            }))
        );
    }

    #[test]
    fn parse_set() {
        let a = Token::try_from_u8(b'a').unwrap();
        let b = Token::try_from_u8(b'b').unwrap();
        let c = Token::try_from_u8(b'c').unwrap();
        let input = vec![Token::OpenBracket, a, b, c, Token::CloseBracket];
        let mut parser = Parser(input.into_iter().peekable(), 0);
        let result = parser.parse_regex();
        assert_eq!(result, Ok(Pattern::Set(vec![a, b, c])));
    }

    #[test]
    fn simple_expr() {
        let a = Token::try_from_u8(b'a').unwrap();
        let b = Token::try_from_u8(b'b').unwrap();
        let input = vec![a, Token::Pipe, b];
        let mut parser = Parser(input.into_iter().peekable(), 0);
        let result = parser.parse_regex();
        assert_eq!(
            result,
            Ok(Pattern::Alternate(vec![
                Pattern::Literal(a),
                Pattern::Literal(b),
            ]))
        );
    }

    #[test]
    fn repeater() {
        let a = Token::try_from_u8(b'a').unwrap();
        let input = vec![a, Token::Question];
        let mut parser = Parser(input.into_iter().peekable(), 0);
        let result = parser.parse_regex();
        assert_eq!(
            result,
            Ok(Pattern::Repeat(
                Box::new(Pattern::Literal(a)),
                Repeater::ZeroOrOne(true)
            ))
        );
    }

    #[test]
    fn repeater_lazy() {
        let a = Token::try_from_u8(b'a').unwrap();
        let input = vec![a, Token::Question, Token::Question];
        let mut parser = Parser(input.into_iter().peekable(), 0);
        let result = parser.parse_regex();
        assert_eq!(
            result,
            Ok(Pattern::Repeat(
                Box::new(Pattern::Literal(a)),
                Repeater::ZeroOrOne(false)
            ))
        );
    }

    #[test]
    fn unexpected_question() {
        let a = Token::try_from_u8(b'a').unwrap();
        let input = vec![a, Token::Question, Token::Question, Token::Question];
        let mut parser = Parser(input.into_iter().peekable(), 0);
        let result = parser.parse_regex();
        assert_eq!(result, Err(Error::Token(3, Token::Question)));
    }

    #[test]
    fn unexpected_open() {
        let a = Token::try_from_u8(b'a').unwrap();
        let input = vec![a, Token::OpenParen];
        let mut parser = Parser(input.into_iter().peekable(), 0);
        let result = parser.parse_regex();
        assert_eq!(result, Err(Error::EndOfInput));
    }

    #[test]
    fn unexpected_open_2() {
        let a = Token::try_from_u8(b'a').unwrap();
        let b = Token::try_from_u8(b'b').unwrap();
        let input = vec![a, Token::Pipe, b, Token::OpenParen];
        let mut parser = Parser(input.into_iter().peekable(), 0);
        let result = parser.parse_regex();
        assert_eq!(result, Err(Error::EndOfInput));
    }

    #[test]
    fn unexpected_open_3() {
        let a = Token::try_from_u8(b'a').unwrap();
        let input = vec![a, Token::Pipe, Token::OpenParen];
        let mut parser = Parser(input.into_iter().peekable(), 0);
        let result = parser.parse_regex();
        assert_eq!(result, Err(Error::EndOfInput));
    }

    #[test]
    fn nested() {
        let a = Token::try_from_u8(b'a').unwrap();
        let input = vec![Token::OpenParen, a, Token::CloseParen];
        let mut parser = Parser(input.into_iter().peekable(), 0);
        let result = parser.parse_regex();
        assert_eq!(result, Ok(Pattern::Literal(a)));
    }

    #[test]
    fn missing_close() {
        let a = Token::try_from_u8(b'a').unwrap();
        let input = vec![Token::OpenParen, a];
        let mut parser = Parser(input.into_iter().peekable(), 0);
        let result = parser.parse_regex();
        assert_eq!(result, Err(Error::EndOfInput));
    }

    #[test]
    fn doubly_nested() {
        let a = Token::try_from_u8(b'a').unwrap();
        let input = vec![
            Token::OpenParen,
            Token::OpenParen,
            a,
            Token::CloseParen,
            Token::CloseParen,
        ];
        let mut parser = Parser(input.into_iter().peekable(), 0);
        let result = parser.parse_regex();
        assert_eq!(result, Ok(Pattern::Literal(a)));
    }

    #[test]
    fn doubly_nested_mismatch() {
        let a = Token::try_from_u8(b'a').unwrap();
        let input = vec![Token::OpenParen, Token::OpenParen, a, Token::CloseParen];
        let mut parser = Parser(input.into_iter().peekable(), 0);
        let result = parser.parse_regex();
        assert_eq!(result, Err(Error::EndOfInput));
    }

    #[test]
    fn arrow() {
        let a = Token::try_from_u8(b'a').unwrap();
        let input = vec![a, Token::Arrow];
        let mut parser = Parser(input.into_iter().peekable(), 0);
        let result = parser.parse_regex();
        assert_eq!(result, Ok(Pattern::Literal(a)));
    }
}

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

    /// Parses a regular expression.
    fn parse_regex(&mut self) -> Result<Pattern, Error> {
        let mut terms = Vec::new();
        terms.push(self.parse_term()?);
        while let Some(&Token::Pipe) = self.peek() {
            self.next();
            terms.push(self.parse_term()?);
        }
        if terms.len() == 1 {
            terms.into_iter().next().ok_or_else(|| unreachable!())
        } else {
            Ok(Pattern::Alternate(terms))
        }
    }

    /// Parses a term of a regular expression, consisting of a string of atoms, possibly with
    /// repeaters.
    fn parse_term(&mut self) -> Result<Pattern, Error> {
        let mut elems = Vec::new();
        while let Some(&tok) = self.peek() {
            match tok {
                // these can never be a part of an element and should be passed up to the caller.
                Token::Pipe
                | Token::CloseParen
                | Token::Arrow
                | Token::Underscore
                | Token::Slash
                | Token::Exclam
                | Token::And
                | Token::Tab
                | Token::Space => break,
                // otherwise, parse an atom
                _ => elems.push({
                    let atom = self.parse_atom()?;
                    match self.parse_repeater() {
                        Some(rep) => Pattern::Repeat(Box::new(atom), rep),
                        None => atom,
                    }
                }),
            }
        }
        if elems.len() == 1 {
            elems.into_iter().next().ok_or_else(|| unreachable!())
        } else {
            Ok(Pattern::Concat(elems))
        }
    }

    /// Parses a single atom of a regular expression.
    ///
    /// This can be one of the following:
    ///
    /// - `.`, matching any character
    /// - `#`, matching a word boundary
    /// - `$`, matching a syllable boundary
    /// - A category
    /// - A single token
    /// - A set of tokens, enclosed in `[` `]`
    /// - A regular expression, enclosed in `(` `)`
    fn parse_atom(&mut self) -> Result<Pattern, Error> {
        match self.next() {
            Some(Token::Dot) => Ok(Pattern::Any),
            Some(Token::Hash) => Ok(Pattern::WordBoundary),
            Some(Token::Dollar) => Ok(Pattern::SyllableBoundary),
            Some(Token::OpenBrace) => self.parse_category(),
            Some(Token::OpenBracket) => self.parse_set(),
            Some(Token::OpenParen) => {
                let value = self.parse_regex();
                match self.next() {
                    Some(Token::CloseParen) => value,
                    Some(_) => unreachable!("in parse_atom/OpenParen"),
                    None => Err(Error::EndOfInput),
                }
            }
            Some(tok) if tok.is_control_token() => Err(Error::Token(self.index() - 1, tok)),
            Some(tok) => Ok(Pattern::Literal(tok)),
            None => Err(Error::EndOfInput),
        }
    }

    /// Parses a regular expression repeater.
    ///
    /// This can be one of `?`, `*`, or `+`, optionally followed by `?`.
    fn parse_repeater(&mut self) -> Option<Repeater> {
        match self.peek() {
            Some(&Token::Question) => Some(Repeater::ZeroOrOne(self.parse_greed())),
            Some(&Token::Star) => Some(Repeater::ZeroOrMore(self.parse_greed())),
            Some(&Token::Plus) => Some(Repeater::OneOrMore(self.parse_greed())),
            _ => None,
        }
    }

    /// Parses whether a repeater is greedy.
    fn parse_greed(&mut self) -> bool {
        // because we just peeked at it in `parse_repeater`
        self.next();
        if let Some(&Token::Question) = self.peek() {
            self.next();
            false
        } else {
            true
        }
    }

    /// Parses a set of tokens from the stream.
    ///
    /// This should be used _after_ the initial `[` has been popped.
    fn parse_set(&mut self) -> Result<Pattern, Error> {
        let mut toks = Vec::new();
        while let Some(&tok) = self.peek() {
            if let Token::CloseBracket = tok {
                self.next();
                return Ok(Pattern::Set(toks));
            } else if tok.is_control_token() {
                return Err(Error::Token(self.index() - 1, tok));
            } else {
                self.next();
                toks.push(tok);
            }
        }
        Err(Error::EndOfInput)
    }

    /// Parses a category from the stream.
    ///
    /// This should be used _after_ the initial `{` has been popped.
    fn parse_category(&mut self) -> Result<Pattern, Error> {
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
    fn finish_parse_category(&mut self, number: Option<u8>) -> Result<Pattern, Error> {
        let name = self.parse_ident();
        match self.next() {
            Some(Token::CloseBrace) => Ok(Pattern::Category(Category { name, number })),
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
        // peek at the next token
        while let Some(&tok) = self.peek() {
            if !tok.is_control_token() {
                // if it's not a control token, it's part of the ident
                id.push(tok);
                self.next();
            } else {
                // otherwise, we're done
                break;
            }
        }
        id
    }
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
