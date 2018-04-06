use token::Token;

use super::re::{Instr, Program, RegexExtension};

enum Chunker {
    /// Matches a single token.
    Char(Token),
    /// Matches a non-control character.
    Normal,
    /// Matches a regular expression control character.
    Regex,
    /// Matches a whitespace character.
    Space,
}

impl Chunker {
    fn is_regex(tok: Token) -> bool {
        match tok {
            Token::Dot
            | Token::Star
            | Token::Plus
            | Token::Question
            | Token::OpenParen
            | Token::CloseParen
            | Token::OpenBracket
            | Token::CloseBracket
            | Token::Pipe
            | Token::Hash
            | Token::Dollar
            | Token::Zero
            | Token::OpenBrace
            | Token::CloseBrace => true,
            _ => false,
        }
    }
}

impl RegexExtension for Chunker {
    type Token = Token;

    fn is_match(&self, tok: Token) -> bool {
        use self::Chunker::*;
        match *self {
            Char(ch) => tok == ch,
            Normal => !tok.is_control_token(),
            Regex => Chunker::is_regex(tok),
            Space => !tok.is_whitespace(),
        }
    }
}

/// Pushes a set of instructions that match an identifier, saving the start and end of the match.
///
/// Equivalent to the Regex `(\w+?)`, where `\w` represents a non-control token. It is non-greedy,
/// so it should only be used when followed by a control token.
fn ident_matcher(prog: &mut Vec<Instr<Chunker>>) {
    use self::Chunker::*;

    let start = prog.len();
    // match /(\w+?)/
    prog.push(Instr::Save); // +00
    prog.push(Instr::Token(Normal)); // +01
    prog.push(Instr::Split(start + 1)); // +02
    prog.push(Instr::Save); // +03
}

/// Pushes a set of instructions that match any number of whitespace characters, without saving the
/// endpoints.
fn whitespace_matcher(prog: &mut Vec<Instr<Chunker>>) {
    use self::Chunker::*;

    let start = prog.len();
    // match /(\s*)/
    prog.push(Instr::Split(start + 3)); // +00
    prog.push(Instr::Token(Normal)); // +01
    prog.push(Instr::Jump(start)); // +02
}
