use token::Token;

use super::re::{Instr, Program, RegexExtension};

pub enum Chunker {
    /// Matches a single token.
    Char(Token),
    /// Matches a non-control character.
    Normal,
    /// Matches a regular expression control character.
    Regex,
    /// Matches a non-control character or a regex control character.
    NormalOrRegex,
    /// Matches a digit 0-9.
    Digit,
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
            | Token::Zero => true,
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
            NormalOrRegex => Chunker::is_regex(tok) || !tok.is_control_token(),
            Digit => tok.is_digit(),
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

/// Pushes an instruction that matches a single `Token` and saves its start and endpoints.
fn token_matcher(prog: &mut Vec<Instr<Chunker>>, tok: Token) {
    prog.push(Instr::Save);
    prog.push(Instr::Token(Chunker::Char(tok)));
    prog.push(Instr::Save);
}

/// Pushes a set of instructions that match one or more valid Regex tokens (including categories),
/// and saves the start and endpoints of each.
fn regex_matcher(prog: &mut Vec<Instr<Chunker>>) {
    use self::Chunker::*;

    let start = prog.len();
    prog.push(Instr::Save); // +00
    prog.push(Instr::Split(start + 4)); // +01
    prog.push(Instr::Token(NormalOrRegex)); // +02
    prog.push(Instr::Jump(0)); // +03
    prog.push(Instr::Token(Char(Token::OpenBrace))); // +04
    prog.push(Instr::Split(start + 10)); // +05
    prog.push(Instr::Token(Digit)); // +07
    prog.push(Instr::Split(start + 7)); // +08
    prog.push(Instr::Token(Char(Token::Colon))); // +09
    prog.push(Instr::Token(Normal)); // +0A
    prog.push(Instr::Split(start + 10)); // +0B
    prog.push(Instr::Token(Char(Token::CloseBrace))); // +0C
    prog.push(Instr::Split(start)); // +0D
    prog.push(Instr::Save); // +0E
}

/// Construct a regex program to match a potentially valid statement, saving the start and
/// endpoints of each chunk.
pub fn matcher() -> Program<Chunker> {
    use self::Chunker::*;

    let mut prog = Vec::new();
    // allow leading whitespace
    whitespace_matcher(&mut prog);
    let mut split = prog.len();
    prog.push(Instr::Split(0));
    let match_end = prog.len();
    // allow empty lines
    prog.push(Instr::MatchEnd);
    prog[split] = Instr::Split(prog.len());
    split = prog.len();
    prog.push(Instr::Split(0));
    // allow line-initial comments starting with "//"
    prog.push(Instr::Token(Char(Token::Slash)));
    prog.push(Instr::Token(Char(Token::Slash)));
    prog.push(Instr::Match);
    prog[split] = Instr::Split(prog.len());
    split = prog.len();
    prog.push(Instr::Split(0));
    // match a category definition
    // category name
    ident_matcher(&mut prog);
    whitespace_matcher(&mut prog);
    token_matcher(&mut prog, Token::Equals);
    // that's enough to identify a category definition, and we can handle the parsing of that later
    prog.push(Instr::Match);
    prog[split] = Instr::Split(prog.len());
    // match a rule
    regex_matcher(&mut prog);
    whitespace_matcher(&mut prog);
    token_matcher(&mut prog, Token::Arrow);
    whitespace_matcher(&mut prog);
    regex_matcher(&mut prog);
    whitespace_matcher(&mut prog);
    prog.push(Instr::Split(match_end));
    split = prog.len();
    prog.push(Instr::Split(0));
    token_matcher(&mut prog, Token::Slash);
    whitespace_matcher(&mut prog);
    unimplemented!();
}
