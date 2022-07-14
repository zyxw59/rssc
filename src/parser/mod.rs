//! Functions for parsing a sound change file.

use std::borrow::Borrow;

use crate::token::Token;

pub mod rule;

/// Determines what kind of statement a line represents based on simple criteria.
///
/// - If the line is empty, or all whitespace, it is an empty line.
/// - If the first non-whitespace character is '/', the line is a comment.
/// - If the line contains one of '=' or '>', whichever one it encounters
///   first determines whether it will be treated as a category definition
///   ('=') or a rule ('>').
/// - Otherwise, if the line does not contain one of '=' or '>', the line is
///   invalid.
fn match_line<I, T>(line: &mut I) -> StatementType
where
    I: Iterator<Item = T>,
    T: Borrow<Token>,
{
    let mut line = line.peekable();
    while let Some(tok) = line.peek().map(|t| *t.borrow()) {
        match tok {
            Token::Newline | Token::Tab | Token::Space => {
                line.next();
            }
            _ => {
                break;
            }
        }
    }
    match line.peek().map(|t| *t.borrow()) {
        None => StatementType::Null,
        Some(Token::Slash) => StatementType::Comment,
        Some(_) => {
            for tok in line {
                match *tok.borrow() {
                    Token::Equals => {
                        return StatementType::Category;
                    }
                    Token::Arrow => {
                        return StatementType::Rule;
                    }
                    _ => {}
                }
            }
            StatementType::Error
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum StatementType {
    /// An empty line.
    Null,
    /// A comment.
    Comment,
    /// A category definition.
    Category,
    /// A rule.
    Rule,
    /// An invalid line.
    Error,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn null_line() {
        let line = Vec::new();
        let ty = match_line(&mut line.iter());
        assert_eq!(ty, StatementType::Null);
    }

    #[test]
    fn comment_line() {
        let line = [Token::Slash, Token::Space, Token::Star];
        let ty = match_line(&mut line.into_iter());
        assert_eq!(ty, StatementType::Comment);
    }

    #[test]
    fn category_line() {
        let line = [
            Token::try_from_u8(b'a').unwrap(),
            Token::Space,
            Token::Equals,
        ];
        let ty = match_line(&mut line.into_iter());
        assert_eq!(ty, StatementType::Category);
    }

    #[test]
    fn rule_line() {
        let line = [Token::try_from_u8(b'a').unwrap(), Token::Arrow, Token::Zero];
        let ty = match_line(&mut line.into_iter());
        assert_eq!(ty, StatementType::Rule);
    }

    #[test]
    fn error_line() {
        let line = [Token::try_from_u8(b'a').unwrap()];
        let ty = match_line(&mut line.into_iter());
        assert_eq!(ty, StatementType::Error);
    }
}
