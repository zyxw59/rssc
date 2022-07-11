use std::{collections::HashSet, iter::Peekable};

use crate::{
    category::Ident,
    rule::{Category, Environment, Pattern, Repeater, Replace, ReplaceTok, Rule, Search},
    token::Token,
    utils::BooleanExpr,
};

pub struct Parser<I: Iterator> {
    iter: Peekable<I>,
    index: usize,
}

impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    pub fn new(iter: impl IntoIterator<IntoIter = I>) -> Self {
        Parser {
            iter: iter.into_iter().peekable(),
            index: 0,
        }
    }

    fn peek(&mut self) -> Option<&Token> {
        self.iter.peek()
    }

    fn next(&mut self) -> Option<Token> {
        self.iter.next().map(|tok| {
            self.index += 1;
            tok
        })
    }

    /// Returns the number of tokens that have been read so far.
    fn index(&self) -> usize {
        self.index
    }

    /// Skips until the next non-whitespace character.
    fn skip_whitespace(&mut self) {
        while let Some(&tok) = self.peek() {
            if tok.is_whitespace() {
                self.next();
            } else {
                break;
            }
        }
    }

    /// Parses a rule.
    pub fn parse_rule(&mut self) -> Result<Rule, Error> {
        // parse the search
        let search = match self.peek() {
            Some(&Token::Zero) => {
                self.next();
                Ok(Search::Zero)
            }
            Some(_) => self.parse_regex().map(Search::Pattern),
            None => Err(Error::EndOfInput),
        }?;
        self.skip_whitespace();
        // match the `>`
        match self.next() {
            Some(Token::Arrow) => Ok(()),
            Some(tok) => Err(Error::Token(self.index() - 1, tok)),
            None => Err(Error::EndOfInput),
        }?;
        self.skip_whitespace();
        // parse the replace
        let replace = match self.peek() {
            Some(&Token::Zero) => {
                self.next();
                Ok(Replace(Vec::new()))
            }
            Some(_) => self.parse_replace(),
            None => Err(Error::EndOfInput),
        }?;
        self.skip_whitespace();
        // match `/` or `!`
        let environment = match self.next() {
            Some(Token::Slash) => {
                self.skip_whitespace();
                // positive environment
                self.parse_environment()
            }
            Some(Token::Exclam) => {
                self.skip_whitespace();
                // negative environment
                self.parse_environment().map(BooleanExpr::not)
            }
            Some(tok) => Err(Error::Token(self.index() - 1, tok)),
            None => Ok(BooleanExpr::True),
        }?;
        self.skip_whitespace();
        if let Some(tok) = self.next() {
            Err(Error::Token(self.index() - 1, tok))
        } else {
            Ok(Rule {
                search,
                replace,
                environment,
            })
        }
    }

    /// Parses a replacement string in a rule.
    fn parse_replace(&mut self) -> Result<Replace, Error> {
        let mut toks = Vec::new();
        while let Some(&tok) = self.peek() {
            match tok {
                Token::OpenBrace => {
                    self.next();
                    toks.push(ReplaceTok::Category(self.parse_category()?));
                }
                Token::Slash | Token::Exclam => break,
                tok if tok.is_whitespace() => break,
                tok => {
                    self.next();
                    toks.push(ReplaceTok::Token(tok));
                }
            }
        }
        Ok(Replace(toks))
    }

    /// Parses an environment for a rule.
    fn parse_environment(&mut self) -> Result<BooleanExpr<Environment>, Error> {
        self.skip_whitespace();
        match self.peek() {
            Some(&Token::And) => {
                self.next();
                match self.next() {
                    Some(Token::OpenParen) => Ok(first_or_else(
                        self.parse_environment_list()?,
                        BooleanExpr::And,
                    )),
                    Some(tok) => Err(Error::Token(self.index() - 1, tok)),
                    None => Err(Error::EndOfInput),
                }
            }
            Some(&Token::Pipe) => {
                self.next();
                match self.next() {
                    Some(Token::OpenParen) => Ok(first_or_else(
                        self.parse_environment_list()?,
                        BooleanExpr::Or,
                    )),
                    Some(tok) => Err(Error::Token(self.index() - 1, tok)),
                    None => Err(Error::EndOfInput),
                }
            }
            Some(&Token::Exclam) => {
                self.next();
                self.parse_environment().map(BooleanExpr::not)
            }
            Some(_) => {
                let before = self.parse_regex()?;
                self.skip_whitespace();
                let after = match self.next() {
                    Some(Token::Underscore) => {
                        self.skip_whitespace();
                        self.parse_regex()
                    }
                    Some(tok) => Err(Error::Token(self.index() - 1, tok)),
                    None => Err(Error::EndOfInput),
                }?;
                Ok(BooleanExpr::Value(Environment { before, after }))
            }
            None => Ok(BooleanExpr::True),
        }
    }

    /// Parses a list of environments until a close paren (that isn't part of a regex).
    ///
    /// Assumes the open paren has already been popped. This pops the final close paren.
    fn parse_environment_list(&mut self) -> Result<Vec<BooleanExpr<Environment>>, Error> {
        let mut exprs = Vec::new();
        loop {
            self.skip_whitespace();
            match self.peek() {
                Some(&Token::CloseParen) => {
                    self.next();
                    break;
                }
                Some(_) => exprs.push(self.parse_environment()?),
                None => return Err(Error::EndOfInput),
            };
        }
        Ok(exprs)
    }

    /// Parses a regular expression.
    fn parse_regex(&mut self) -> Result<Pattern, Error> {
        let mut terms = vec![self.parse_term()?];
        while let Some(&Token::Pipe) = self.peek() {
            self.next();
            terms.push(self.parse_term()?);
        }
        Ok(first_or_else(terms, Pattern::Alternate))
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
        Ok(first_or_else(elems, Pattern::Concat))
    }

    /// Parses a single atom of a regular expression.
    ///
    /// This can be one of the following:
    ///
    /// - `.`, matching any character
    /// - `#`, matching a word boundary
    /// - A category
    /// - A single token
    /// - A set of tokens, enclosed in `[` `]`
    /// - A regular expression, enclosed in `(` `)`
    fn parse_atom(&mut self) -> Result<Pattern, Error> {
        match self.next() {
            Some(Token::Dot) => Ok(Pattern::Any),
            Some(Token::Hash) => Ok(Pattern::WordBoundary),
            Some(Token::OpenBrace) => self.parse_category().map(Pattern::Category),
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
        let mut toks = HashSet::new();
        while let Some(&tok) = self.peek() {
            if let Token::CloseBracket = tok {
                self.next();
                return Ok(Pattern::Set(toks));
            } else if tok.is_control_token() {
                return Err(Error::Token(self.index() - 1, tok));
            } else {
                self.next();
                toks.insert(tok);
            }
        }
        Err(Error::EndOfInput)
    }

    /// Parses a category from the stream.
    ///
    /// This should be used _after_ the initial `{` has been popped.
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

fn first_or_else<F, T>(mut vec: Vec<T>, or_else: F) -> T
where
    F: FnOnce(Vec<T>) -> T,
{
    if vec.len() == 1 {
        vec.pop().unwrap()
    } else {
        or_else(vec)
    }
}

/// An error encountered during parsing.
#[derive(Clone, Copy, Debug, PartialEq, thiserror::Error)]
pub enum Error {
    /// An unexpected token, and the index it occurred at.
    #[error("Unexpected token `{1:?}` at position {0}")]
    Token(usize, Token),
    /// Unexpected end of input.
    #[error("Unexpected end of input")]
    EndOfInput,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::tokenizer::tokenize;

    #[test]
    fn parse_number() {
        let input = tokenize("123");
        let mut parser = Parser::new(input);
        let num = parser.parse_number();
        assert_eq!(num, 123);
        assert_eq!(parser.next(), None);
    }

    #[test]
    fn parse_ident() {
        let input = tokenize("abc");
        let mut parser = Parser::new(input.clone());
        let ident = parser.parse_ident();
        assert_eq!(ident, input);
    }

    #[test]
    fn parse_category_no_number() {
        let input = tokenize("{C}");
        let mut parser = Parser::new(input);
        let result = parser.parse_regex();
        assert_eq!(
            result,
            Ok(Pattern::Category(Category {
                name: vec![Token(b'C' as u16)],
                number: None,
            }))
        );
    }

    #[test]
    fn parse_category_number() {
        let input = tokenize("{3:C}");
        let mut parser = Parser::new(input);
        let result = parser.parse_regex();
        assert_eq!(
            result,
            Ok(Pattern::Category(Category {
                name: vec![Token(b'C' as u16)],
                number: Some(3),
            }))
        );
    }

    #[test]
    fn parse_set() {
        let input = tokenize("[abc]");
        let mut parser = Parser::new(input);
        let result = parser.parse_regex();
        assert_eq!(
            result,
            Ok(Pattern::Set(
                [Token(b'a' as u16), Token(b'b' as u16), Token(b'c' as u16)]
                    .into_iter()
                    .collect()
            ))
        );
    }

    #[test]
    fn simple_expr() {
        let input = tokenize("a|b");
        let mut parser = Parser::new(input);
        let result = parser.parse_regex();
        assert_eq!(
            result,
            Ok(Pattern::Alternate(vec![
                Pattern::Literal(Token(b'a' as u16)),
                Pattern::Literal(Token(b'b' as u16)),
            ]))
        );
    }

    #[test]
    fn repeater() {
        let input = tokenize("a?");
        let mut parser = Parser::new(input);
        let result = parser.parse_regex();
        assert_eq!(
            result,
            Ok(Pattern::Repeat(
                Box::new(Pattern::Literal(Token(b'a' as u16))),
                Repeater::ZeroOrOne(true)
            ))
        );
    }

    #[test]
    fn repeater_lazy() {
        let input = tokenize("a??");
        let mut parser = Parser::new(input);
        let result = parser.parse_regex();
        assert_eq!(
            result,
            Ok(Pattern::Repeat(
                Box::new(Pattern::Literal(Token(b'a' as u16))),
                Repeater::ZeroOrOne(false)
            ))
        );
    }

    #[test]
    fn unexpected_question() {
        let input = tokenize("a???");
        let mut parser = Parser::new(input);
        let result = parser.parse_regex();
        assert_eq!(result, Err(Error::Token(3, Token::Question)));
    }

    #[test]
    fn unexpected_open() {
        let input = tokenize("a(");
        let mut parser = Parser::new(input);
        let result = parser.parse_regex();
        assert_eq!(result, Err(Error::EndOfInput));
    }

    #[test]
    fn unexpected_open_2() {
        let input = tokenize("a|b(");
        let mut parser = Parser::new(input);
        let result = parser.parse_regex();
        assert_eq!(result, Err(Error::EndOfInput));
    }

    #[test]
    fn unexpected_open_3() {
        let input = tokenize("a|(");
        let mut parser = Parser::new(input);
        let result = parser.parse_regex();
        assert_eq!(result, Err(Error::EndOfInput));
    }

    #[test]
    fn nested() {
        let input = tokenize("(a)");
        let mut parser = Parser::new(input);
        let result = parser.parse_regex();
        assert_eq!(result, Ok(Pattern::Literal(Token(b'a' as u16))));
    }

    #[test]
    fn missing_close() {
        let input = tokenize("(a");
        let mut parser = Parser::new(input);
        let result = parser.parse_regex();
        assert_eq!(result, Err(Error::EndOfInput));
    }

    #[test]
    fn doubly_nested() {
        let input = tokenize("((a))");
        let mut parser = Parser::new(input);
        let result = parser.parse_regex();
        assert_eq!(result, Ok(Pattern::Literal(Token(b'a' as u16))));
    }

    #[test]
    fn doubly_nested_mismatch() {
        let input = tokenize("((a)");
        let mut parser = Parser::new(input);
        let result = parser.parse_regex();
        assert_eq!(result, Err(Error::EndOfInput));
    }

    #[test]
    fn arrow() {
        let input = tokenize("a>");
        let mut parser = Parser::new(input);
        let result = parser.parse_regex();
        assert_eq!(result, Ok(Pattern::Literal(Token(b'a' as u16))));
    }

    #[test]
    fn parse_replace() {
        let input = tokenize("a{1:C}");
        let mut parser = Parser::new(input);
        let result = parser.parse_replace();
        assert_eq!(
            result,
            Ok(Replace(vec![
                ReplaceTok::Token(Token(b'a' as u16)),
                ReplaceTok::Category(Category {
                    name: vec![Token(b'C' as u16)],
                    number: Some(1),
                }),
            ]))
        );
    }

    #[test]
    fn simple_environment() {
        let input = tokenize("a_b");
        let mut parser = Parser::new(input);
        let result = parser.parse_environment();
        assert_eq!(
            result,
            Ok(BooleanExpr::Value(Environment {
                before: Pattern::Literal(Token(b'a' as u16)),
                after: Pattern::Literal(Token(b'b' as u16)),
            }))
        );
    }

    #[test]
    fn environment_or() {
        let input = tokenize("|(a_b c_d)");
        let mut parser = Parser::new(input);
        let result = parser.parse_environment();
        assert_eq!(
            result,
            Ok(BooleanExpr::Or(vec![
                BooleanExpr::Value(Environment {
                    before: Pattern::Literal(Token(b'a' as u16)),
                    after: Pattern::Literal(Token(b'b' as u16))
                }),
                BooleanExpr::Value(Environment {
                    before: Pattern::Literal(Token(b'c' as u16)),
                    after: Pattern::Literal(Token(b'd' as u16))
                }),
            ]))
        );
    }

    #[test]
    fn environment_nested() {
        let input = tokenize("|(&(a_ _b)c_d)");
        let mut parser = Parser::new(input);
        let result = parser.parse_environment();
        assert_eq!(
            result,
            Ok(BooleanExpr::Or(vec![
                BooleanExpr::And(vec![
                    BooleanExpr::Value(Environment {
                        before: Pattern::Literal(Token(b'a' as u16)),
                        after: Pattern::Concat(vec![])
                    }),
                    BooleanExpr::Value(Environment {
                        before: Pattern::Concat(vec![]),
                        after: Pattern::Literal(Token(b'b' as u16))
                    }),
                ]),
                BooleanExpr::Value(Environment {
                    before: Pattern::Literal(Token(b'c' as u16)),
                    after: Pattern::Literal(Token(b'd' as u16))
                }),
            ]))
        );
    }
}
