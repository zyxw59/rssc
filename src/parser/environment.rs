//! A simple recursive descent parser for environment expressssions.
//!
//! Environment expressions are logical expressions that dictate the environments in which a sound
//! change applies (or doesn't apply). The grammar of environment expressions is as follows:
//!
//! ```text
//! Expr : Term ( '|' Term ) *
//! Term : Factor ( '&' Factor ) *
//! Factor : '(' Expr ')' | '!' Factor | Environment
//! Environment : Pattern '_' Pattern
//! ```
//!
//! This module does not handle the parsing of `Environment`s or `Pattern`s themselves, for that,
//! see the `pattern` module.

use std::error;
use std::fmt;
use std::iter::Peekable;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unexpected_close_paren() {
        use self::Token::*;
        let input = vec![Other(0), End];
        let tree = Expr::parse(input.into_iter());
        assert_eq!(tree, Err(Error::CloseParen));
    }

    #[test]
    fn simple_expr() {
        use self::Token::*;
        let input = vec![Other(0), Or, Other(1)];
        let tree = Expr::parse(input.into_iter());
        assert_eq!(tree, Ok(Expr::Or(vec![Expr::Other(0), Expr::Other(1)])));
    }

    #[test]
    fn unexpected_or() {
        use self::Token::*;
        let input: Vec<Token<()>> = vec![Not, Or];
        let tree = Expr::parse(input.into_iter());
        assert_eq!(tree, Err(Error::Or));
    }

    #[test]
    fn unexpected_not() {
        use self::Token::*;
        let input = vec![Other(0), Or, Not];
        let tree = Expr::parse(input.into_iter());
        assert_eq!(tree, Err(Error::EndOfInput));
    }

    #[test]
    fn unexpected_not_2() {
        use self::Token::*;
        let input = vec![Other(0), Not, Or, Other(1)];
        let tree = Expr::parse(input.into_iter());
        assert_eq!(tree, Err(Error::Not));
    }

    #[test]
    fn unexpected_not_3() {
        use self::Token::*;
        let input = vec![Begin, Other(0), Or, Other(1), Not];
        let tree = Expr::parse(input.into_iter());
        assert_eq!(tree, Err(Error::Not));
    }

    #[test]
    fn unexpected_open() {
        use self::Token::*;
        let input = vec![Other(0), Begin];
        let tree = Expr::parse(input.into_iter());
        assert_eq!(tree, Err(Error::OpenParen));
    }

    #[test]
    fn unexpected_open_2() {
        use self::Token::*;
        let input = vec![Other(0), Or, Other(1), Begin];
        let tree = Expr::parse(input.into_iter());
        assert_eq!(tree, Err(Error::OpenParen));
    }

    #[test]
    fn unexpected_open_3() {
        use self::Token::*;
        let input = vec![Other(0), Or, Begin];
        let tree = Expr::parse(input.into_iter());
        assert_eq!(tree, Err(Error::EndOfInput));
    }

    #[test]
    fn missing_close() {
        use self::Token::*;
        let input = vec![Begin, Other(0)];
        let tree = Expr::parse(input.into_iter());
        assert_eq!(tree, Err(Error::EndOfInput));
    }

    #[test]
    fn doubly_nested() {
        use self::Token::*;
        let input = vec![Begin, Begin, Other(0), End, End];
        let tree = Expr::parse(input.into_iter());
        assert_eq!(tree, Ok(Expr::Other(0)));
    }

    #[test]
    fn doubly_nested_mismatch() {
        use self::Token::*;
        let input = vec![Begin, Begin, Other(0), End];
        let tree = Expr::parse(input.into_iter());
        assert_eq!(tree, Err(Error::EndOfInput));
    }
}

/// A token to be read by the parser.
///
/// This type is made generic over the type of the `Other` variant for ease of testing and to
/// separate out details of implementation between different parts of the program.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Token<T> {
    /// The character '|'.
    Or,
    /// The character '&'.
    And,
    /// The character '!'.
    Not,
    /// The character '('.
    Begin,
    /// The character ')'.
    End,
    /// Any other token. In the case of parsing an environment expression, `T` should be a type
    /// that represents a single environment.
    Other(T),
}

/// An expression tree produced by the parser.
///
/// This type is made generic over the type of the `Other` variant for ease of testing and to
/// separate out details of implementation between different parts of the program.
#[derive(Clone, Debug, PartialEq)]
pub enum Expr<T> {
    /// True iff _any_ of the expressions are true.
    Or(Vec<Expr<T>>),
    /// True iff _all_ of the expressions are true.
    And(Vec<Expr<T>>),
    /// True iff the expression is false.
    Not(Box<Expr<T>>),
    /// True iff the environment matches.
    Other(T),
}

impl<T> Expr<T> where {
    /// Parse the given stream as an expression.
    ///
    /// If `stream` does not represent a valid expression, an `Err` will be returned describing
    /// the first unexpected token encountered. Otherwise, an `Ok` value will hold the expression
    /// tree produced.
    pub fn parse<I>(stream: I) -> ParseResult<T> where I: Iterator<Item=Token<T>> {
        let mut stream = stream.peekable();
        Expr::expr(&mut stream).and_then(|e| {
            match stream.next() {
                Some(Token::End) => Err(Error::CloseParen),
                Some(Token::Not) => Err(Error::Not),
                Some(Token::Begin) => Err(Error::OpenParen),
                Some(Token::Other(_)) => Err(Error::Other),
                Some(Token::Or) | Some(Token::And) => unreachable!("in parse/Some(Or|And)"),
                None => Ok(e),
            }
        })
    }

    fn expr<I>(stream: &mut Peekable<I>) -> ParseResult<T> where I: Iterator<Item=Token<T>> {
        let mut summands = Vec::new();
        summands.push(Expr::term(stream)?);
        while let Some(&Token::Or) = stream.peek() {
            stream.next();
            summands.push(Expr::term(stream)?);
        }
        if summands.len() == 1 {
            summands.into_iter().next().ok_or_else(|| unreachable!())
        } else {
            Ok(Expr::Or(summands))
        }
    }

    fn term<I>(stream: &mut Peekable<I>) -> ParseResult<T> where I: Iterator<Item=Token<T>> {
        let mut factors = Vec::new();
        factors.push(Expr::factor(stream)?);
        while let Some(&Token::And) = stream.peek() {
            stream.next();
            factors.push(Expr::factor(stream)?);
        }
        if factors.len() == 1 {
            factors.into_iter().next().ok_or_else(|| unreachable!())
        } else {
            Ok(Expr::And(factors))
        }
    }

    fn factor<I>(stream: &mut Peekable<I>) -> ParseResult<T> where I: Iterator<Item=Token<T>> {
        match stream.next() {
            Some(Token::Not) => Ok(Expr::Not(Box::new(Expr::factor(stream)?))),
            Some(Token::Begin) => {
                let value = Expr::expr(stream);
                match stream.next() {
                    Some(Token::End) => value,
                    Some(Token::Not) => Err(Error::Not),
                    Some(Token::Begin) => Err(Error::OpenParen),
                    Some(Token::Other(_)) => Err(Error::Other),
                    Some(Token::Or) | Some(Token::And) => unreachable!("in factor/Begin"),
                    None => Err(Error::EndOfInput),
                }
            },
            Some(Token::Or) => Err(Error::Or),
            Some(Token::And) => Err(Error::And),
            Some(Token::End) => Err(Error::CloseParen),
            Some(Token::Other(tok)) => Ok(Expr::Other(tok)),
            None => Err(Error::EndOfInput),
        }
    }
}

type ParseResult<T> = Result<Expr<T>, Error>;

/// An error encountered in the parsing of an expression.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Error {
    /// An unexpected '|' token.
    Or,
    /// An unexpected '&' token.
    And,
    /// An unexpected '!' token.
    Not,
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
            Other => "Unexpected token",
            CloseParen => "Unexpected ')'",
            OpenParen => "Unexpected '('",
            EndOfInput => "Unexpected end of input",
        }
    }
}

