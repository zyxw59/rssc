//! A regex-based tokenizer which takes an input `char` stream and outputs a `Token` stream.

use std::fmt;
use std::io::{self, BufRead};
use std::iter::Peekable;

use unicode_normalization::UnicodeNormalization;

use crate::{
    token::{Segment, SegmentMap, Token},
    unicode::{is_modifier, is_combining_double},
};

use super::re::{Instr, Program, RegexExtension};

#[cfg(test)]
mod tests {
    use super::*;
    use unicode_normalization::UnicodeNormalization;

    #[test]
    fn multiple_possible_segmentations() {
        let line = String::from("antś\n");
        let line = line.chars().nfd();
        let segments =
            SegmentMap::clone_from_vec(&vec![vec!['a', 'n'], vec!['n', 't'], vec!['t', 's']]);
        let prog = matcher(&segments);
        println!("{prog}");

        let saves = prog.exec(line);
        assert_eq!(saves, Some(vec![0, 2, 3, 5, 6]));
    }

    #[test]
    fn combining_double() {
        let line = String::from("t͜s\n");
        let line = line.chars().nfd();
        let segments = SegmentMap::clone_from_vec(&Vec::new());
        let prog = matcher(&segments);
        println!("{prog}");

        let saves = prog.exec(line);
        assert_eq!(saves, Some(vec![0, 3, 4]));
    }

    #[test]
    fn backslash() {
        let line = String::from("\\.\n");
        let line = line.chars().nfd();
        let segments = SegmentMap::clone_from_vec(&Vec::new());
        let prog = matcher(&segments);
        println!("{prog}");

        let saves = prog.exec(line);
        assert_eq!(saves, Some(vec![0, 2, 3]));
    }

    #[test]
    fn backslash_newline() {
        let line = String::from("\\\n");
        let line = line.chars().nfd();
        let segments = SegmentMap::clone_from_vec(&Vec::new());
        let prog = matcher(&segments);
        println!("{prog}");

        let saves = prog.exec(line);
        assert_eq!(saves, Some(vec![0, 2]));
    }
}

/// A `RegexExtension` for the tokenizer.
#[derive(Clone, Copy, Debug)]
pub enum TokenizerExtension {
    /// Matches a single char.
    Char(char),
    /// Matches any character.
    Any,
    /// Matches a control character.
    ControlChar,
    /// Matches a non-combining character.
    BaseChar,
    /// Matches a combining character.
    CombiningChar,
    /// Matches a combining double character.
    CombiningDouble,
}

impl RegexExtension for TokenizerExtension {
    type Token = char;

    fn is_match(&self, tok: char) -> bool {
        use self::TokenizerExtension::*;
        match *self {
            Char(ch) => tok == ch,
            Any => true,
            ControlChar => Token::is_control_char(tok),
            BaseChar => !is_modifier(tok),
            CombiningChar => is_modifier(tok) && !is_combining_double(tok),
            CombiningDouble => is_combining_double(tok),
        }
    }
}

impl fmt::Display for TokenizerExtension {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

/// Construct a regex program to split a line into segments.
pub fn matcher(segments: &SegmentMap) -> Program<TokenizerExtension> {
    // the instructions
    let mut prog = Vec::new();

    use self::TokenizerExtension::*;

    // save instruction, to be performed at the end of each token
    prog.push(Instr::Save);
    // match a control character
    let mut split = prog.len();
    prog.push(Instr::Split(0));
    prog.push(Instr::Token(ControlChar));
    prog.push(Instr::Jump(0));
    // match a backslash-escaped character, which is considered as a single base character
    prog[split] = Instr::Split(prog.len());
    split = prog.len();
    prog.push(Instr::Split(0));
    prog.push(Instr::Token(Char('\\')));
    prog.push(Instr::Split(split + 5));
    prog.push(Instr::Token(Char('\n')));
    prog.push(Instr::Jump(0));
    prog.push(Instr::Token(Any));
    let escape_jump = prog.len();
    prog.push(Instr::Jump(0));
    // match a newline, and therefore the end of the string
    prog[split] = Instr::Split(prog.len());
    split = prog.len();
    prog.push(Instr::Split(0));
    prog.push(Instr::Token(Char('\n')));
    prog[escape_jump - 2] = Instr::Jump(prog.len());
    prog.push(Instr::Save);
    prog.push(Instr::Match);
    // match user-defined segments
    for seg in segments.iter() {
        prog[split] = Instr::Split(prog.len());
        split = prog.len();
        prog.push(Instr::Split(0));
        for c in seg {
            prog.push(Instr::Token(Char(*c)));
        }
        prog.push(Instr::Jump(0));
    }
    // match a base character followed by any number of combining characters
    let base = prog.len();
    prog[escape_jump] = Instr::Jump(base + 1);
    prog[split] = Instr::Split(base);
    prog.push(Instr::Token(BaseChar));
    prog.push(Instr::Split(base + 4));
    prog.push(Instr::Token(CombiningChar));
    prog.push(Instr::Jump(base + 1));
    prog.push(Instr::Split(0));
    prog.push(Instr::Token(CombiningDouble));
    prog.push(Instr::Jump(base));

    Program::new(prog, 0)
}

/// An `Iterator` that produces the tokens found in a `BufRead`.
#[derive(Debug)]
pub struct Tokens<R> {
    input: R,
    in_buffer: String,
    out_buffer: Vec<Token>,
    index: usize,
    line: usize,
    token_map: Vec<Segment>,
    segment_map: SegmentMap,
    re: Program<TokenizerExtension>,
}

impl<R: BufRead> Tokens<R> {
    pub fn new(input: R, segment_map: SegmentMap) -> Tokens<R> {
        let re = matcher(&segment_map);
        Tokens {
            input,
            in_buffer: String::new(),
            out_buffer: Vec::new(),
            index: 0,
            line: 0,
            token_map: Vec::new(),
            segment_map,
            re,
        }
    }

    /// Fills the internal incoming and outgoing buffers
    fn fill_buffer(&mut self) -> Result<(), Error> {
        // only fill buffer if necessary
        if self.index >= self.out_buffer.len() {
            // reset index
            self.index = 0;
            // reset output buffer
            self.out_buffer.clear();
            // reset input buffer
            self.in_buffer.clear();
            // read a new line
            self.input.read_line(&mut self.in_buffer)?;
            // perform canonical decomposition on the string
            let chars = self.in_buffer.chars().nfd().collect::<Vec<char>>();
            if chars.is_empty() {
                return Ok(());
            }
            // extract segment boundaries
            let saves = self
                .re
                .exec(chars.iter())
                .ok_or(Error::Tokenizing(self.line))?;
            self.line += 1;
            for (start, end) in saves.iter().zip(saves[1..].iter()) {
                // extract the sgement
                let seg = Segment::from(&chars[*start..*end]);
                // get the token corresponding to the segment
                let tok = self.segment_map.get_or_insert(seg.clone());
                // if the segment was new, push it into the token map
                if tok >= Token::from_index(self.token_map.len()) {
                    self.token_map.push(seg);
                }
                // push the token into the output buffer
                self.out_buffer.push(tok);
            }
        }
        Ok(())
    }

    /// An iterator over the lines of the iterator. Lines are ended with `Token::Newline`.
    pub fn lines(self) -> TokenLines<R> {
        TokenLines(self.peekable())
    }
}

impl<R: BufRead> Iterator for Tokens<R> {
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Result<Token, Error>> {
        match self.fill_buffer() {
            Err(e) => Some(Err(e)),
            Ok(()) => match self.out_buffer.get(self.index) {
                Some(t) => {
                    self.index += 1;
                    Some(Ok(*t))
                }
                None => None,
            },
        }
    }
}

/// An iterator over the lines of a `Tokens` iterator. Generated by the `lines` method on `Tokens`.
pub struct TokenLines<R: BufRead>(Peekable<Tokens<R>>);

impl<R: BufRead> Iterator for TokenLines<R> {
    type Item = Result<Vec<Token>, Error>;

    fn next(&mut self) -> Option<Result<Vec<Token>, Error>> {
        // `.and(Some(()))` is needed to end the borrow of `self` created by `peek()`
        self.0.peek().and(Some(())).map(|()| {
            self.0
                // `by_ref()` is needed because `.take_while()` consumes `self`.
                .by_ref()
                .take_while(|t| !matches!(t, Ok(Token::Newline)))
                // this `.collect()` collects into `Result<Vec<Token>, Error>`
                .collect()
        })
    }
}

/// An error encountered during tokenizing.
#[derive(Debug, thiserror::Error)]
pub enum Error {
    /// The specified line lacked a valid tokenization.
    #[error("No valid tokenization of line {0}")]
    Tokenizing(usize),
    /// The specified IO error occurred.
    #[error(transparent)]
    IO(#[from] io::Error),
}
