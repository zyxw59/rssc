//! A simple recursive descent parser for regulr expressssions.
//!
//! The grammar of regular expressions used by this program is as follows:
//!
//! ```text
//! Expr : Term ( '|' Term ) *
//! Term : ( Elem Repeater ) *
//! Elem : '.' | '#' | '$' | Category | Token | '(' Expr ')'
//! Repeater : ( '?' | '*' | '+' ) '?' ?
//! ```

use std::error;
use std::fmt;
use std::iter::Peekable;

use ast::Repeater;

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
        assert_eq!(tree, Ok(Expr::Alternate(vec![Expr::Other(0), Expr::Other(1)])));
    }

    #[test]
    fn repeater() {
        use self::Token::*;
        let input = vec![Other(0), Question];
        let tree = Expr::parse(input.into_iter());
        assert_eq!(tree, Ok(Expr::Repeat(Box::new(Expr::Other(0)), Repeater::ZeroOrOne(true))));
    }

    #[test]
    fn repeater_lazy() {
        use self::Token::*;
        let input = vec![Other(0), Question, Question];
        let tree = Expr::parse(input.into_iter());
        assert_eq!(tree, Ok(Expr::Repeat(Box::new(Expr::Other(0)), Repeater::ZeroOrOne(false))));
    }

    #[test]
    fn unexpected_question() {
        use self::Token::*;
        let input = vec![Other(0), Question, Question, Question];
        let tree = Expr::parse(input.into_iter());
        assert_eq!(tree, Err(Error::Question));
    }

    #[test]
    fn unexpected_star() {
        use self::Token::*;
        let input = vec![Other(0), Question, Star];
        let tree = Expr::parse(input.into_iter());
        assert_eq!(tree, Err(Error::Star));
    }

    #[test]
    fn unexpected_open() {
        use self::Token::*;
        let input = vec![Other(0), Begin];
        let tree = Expr::parse(input.into_iter());
        assert_eq!(tree, Err(Error::EndOfInput));
    }

    #[test]
    fn unexpected_open_2() {
        use self::Token::*;
        let input = vec![Other(0), Or, Other(1), Begin];
        let tree = Expr::parse(input.into_iter());
        assert_eq!(tree, Err(Error::EndOfInput));
    }

    #[test]
    fn unexpected_open_3() {
        use self::Token::*;
        let input = vec![Other(0), Or, Begin];
        let tree = Expr::parse(input.into_iter());
        assert_eq!(tree, Err(Error::EndOfInput));
    }

    #[test]
    fn nested() {
        use self::Token::*;
        let input = vec![Begin, Other(0), End];
        let tree = Expr::parse(input.into_iter());
        assert_eq!(tree, Ok(Expr::Other(0)));
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
    /// The character '('.
    Begin,
    /// The character ')'.
    End,
    /// The character '?'.
    Question,
    /// The character '*'.
    Star,
    /// The character '+'.
    Plus,
    /// Any single token, including categories and word and syllable boundaries.
    Other(T),
}

/// An expression tree produced by the parser.
///
/// This type is made generic over the type of the `Other` variant for ease of testing and to
/// separate out details of implementation between different parts of the program.
#[derive(Clone, Debug, PartialEq)]
pub enum Expr<T> {
    /// Matches one of multiple patterns.
    Alternate(Vec<Expr<T>>),
    /// Matches the concatenation of a list of patterns.
    Concatenate(Vec<Expr<T>>),
    /// Matches a repetition of a pattern.
    Repeat(Box<Expr<T>>, Repeater),
    /// Any single token.
    Other(T)
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
                Some(_) => unreachable!("in parse/Some(_)"),
                None => Ok(e),
            }
        })
    }

    fn expr<I>(stream: &mut Peekable<I>) -> ParseResult<T> where I: Iterator<Item=Token<T>> {
        let mut terms = Vec::new();
        terms.push(Expr::term(stream)?);
        while let Some(&Token::Or) = stream.peek() {
            stream.next();
            terms.push(Expr::term(stream)?);
        }
        if terms.len() == 1 {
            terms.into_iter().next().ok_or_else(|| unreachable!())
        } else {
            Ok(Expr::Alternate(terms))
        }
    }

    fn term<I>(stream: &mut Peekable<I>) -> ParseResult<T> where I: Iterator<Item=Token<T>> {
        let mut factors = Vec::new();
        while match stream.peek() {
            Some(&Token::Or) | Some(&Token::End) | None => false,
            Some(_) => true,
        } {
            factors.push({
                let elem = Expr::elem(stream)?;
                match Expr::repeater(stream) {
                    Some(rep) => Expr::Repeat(Box::new(elem), rep),
                    None => elem,
                }
            });
        }
        if factors.len() == 1 {
            factors.into_iter().next().ok_or_else(|| unreachable!())
        } else {
            Ok(Expr::Concatenate(factors))
        }
    }

    fn elem<I>(stream: &mut Peekable<I>) -> ParseResult<T> where I: Iterator<Item=Token<T>> {
        match stream.next() {
            Some(Token::Begin) => {
                let value = Expr::expr(stream);
                match stream.next() {
                    Some(Token::End) => value,
                    Some(_) => unreachable!("in elem/Begin"),
                    None => Err(Error::EndOfInput),
                }
            },
            Some(Token::Or) => unreachable!("in elem/Or"),
            Some(Token::Question) => Err(Error::Question),
            Some(Token::Star) => Err(Error::Star),
            Some(Token::Plus) => Err(Error::Plus),
            Some(Token::End) => unreachable!("in elem/End"),
            Some(Token::Other(tok)) => Ok(Expr::Other(tok)),
            None => unreachable!("in elem/None"),
        }
    }

    fn repeater<I>(stream: &mut Peekable<I>) -> Option<Repeater> where I: Iterator<Item=Token<T>> {
        match stream.peek() {
            Some(&Token::Question) => {
                Some(Repeater::ZeroOrOne(Expr::repeater_greed(stream)))
            },
            Some(&Token::Star) => {
                Some(Repeater::ZeroOrMore(Expr::repeater_greed(stream)))
            },
            Some(&Token::Plus) => {
                Some(Repeater::OneOrMore(Expr::repeater_greed(stream)))
            },
            _ => None
        }
    }

    fn repeater_greed<I>(stream: &mut Peekable<I>) -> bool where I: Iterator<Item=Token<T>> {
        stream.next();
        if let Some(&Token::Question) = stream.peek() {
            stream.next();
            false
        } else {
            true
        }
    }
}

type ParseResult<T> = Result<Expr<T>, Error>;

/// An error encountered in the parsing of an expression.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Error {
    /// An unexpected '|' token.
    Or,
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

