//! A regex-based tokenizer which takes an input `char` stream and outputs a `Token` stream.

use std::fmt;
use std::hash::{Hash, Hasher};
use std::io::{self, BufRead};

use irregex::{Engine, Instr, Program};
use unicode_normalization::UnicodeNormalization;

use super::{SegmentMap, Token, TokenString};
use crate::unicode::{is_combining_double, is_modifier};

#[derive(Clone, Debug, Default)]
struct TokenizerEngine {
    indices: Vec<usize>,
}

impl Engine for TokenizerEngine {
    type Token = char;
    type Consume = Consume;
    type Peek = Peek;

    fn consume(&mut self, args: &Self::Consume, _index: usize, tok: &Self::Token) -> bool {
        match args {
            Consume::Char(ch) => tok == ch,
            Consume::ControlChar => *tok != '\n' && Token::is_control_char(*tok),
            Consume::BaseChar => {
                !Token::is_control_char(*tok) && *tok != '\\' && !is_modifier(*tok)
            }
            Consume::CombiningChar => is_modifier(*tok) && !is_combining_double(*tok),
            Consume::CombiningDouble => is_combining_double(*tok),
        }
    }

    fn peek(&mut self, args: &Self::Peek, index: usize, token: Option<&char>) -> bool {
        match args {
            Peek::Save => {
                self.indices.push(index);
                true
            }
            Peek::EndOfLine => {
                match token {
                    Some(&'\n') => {
                        // index + 1 because we haven't actually consumed the newline
                        self.indices.push(index + 1);
                        true
                    }
                    None => true,
                    _ => false,
                }
            }
        }
    }

    fn any(&mut self, _index: usize, tok: &Self::Token) -> bool {
        *tok != '\n'
    }
}

impl Hash for TokenizerEngine {
    fn hash<H>(&self, _state: &mut H)
    where
        H: Hasher,
    {
        // nothing to hash — ignore any state
    }
}

/// A `RegexExtension` for the tokenizer.
#[derive(Clone, Copy, Debug)]
enum Consume {
    /// Matches a single char.
    Char(char),
    /// Matches a control character.
    ControlChar,
    /// Matches a non-combining character.
    BaseChar,
    /// Matches a combining character.
    CombiningChar,
    /// Matches a combining double character.
    CombiningDouble,
}

impl fmt::Display for Consume {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

#[derive(Clone, Copy, Debug)]
enum Peek {
    /// Saves the current position as a token boundary.
    Save,
    /// Matches a newline or end of input.
    EndOfLine,
}

impl fmt::Display for Peek {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

/// Construct a regex program to split a line into segments.
fn matcher(segments: &SegmentMap) -> Program<TokenizerEngine> {
    // the instructions
    let mut prog = Program::new();
    // save instruction, to be performed at the end of each token
    prog.push(Instr::Peek(Peek::Save));

    // match a control character
    let mut split = prog.len();
    prog.extend([
        Instr::Split(0), // to be set later
        Instr::Consume(Consume::ControlChar),
        Instr::Jump(0),
    ]);
    prog[split] = Instr::Split(prog.len());
    // match a backslash-escaped character, which is considered as a single base character
    split = prog.len();
    prog.extend([
        Instr::Split(0), // to be set later
        Instr::Consume(Consume::Char('\\')),
        Instr::Split(split + 6),
        Instr::Consume(Consume::Char('\n')),
        Instr::Peek(Peek::Save),
        Instr::Match,
        Instr::Any,
        Instr::Jump(0), // to be set later
    ]);
    let escape_jump = prog.len() - 1;
    prog[split] = Instr::Split(prog.len());
    // match a newline, and therefore the end of the string
    split = prog.len();
    prog.extend([
        Instr::Split(0), // to be set later
        Instr::Peek(Peek::EndOfLine),
        Instr::Match,
    ]);
    // match user-defined segments
    for seg in segments.iter() {
        prog[split] = Instr::Split(prog.len());
        split = prog.len();
        prog.push(Instr::Split(0));
        prog.extend(seg.iter().map(|&c| Instr::Consume(Consume::Char(c))));
        prog.push(Instr::Jump(0));
    }
    // match a base character followed by any number of combining characters
    let base = prog.len();
    prog[split] = Instr::Split(base);
    // base character was already matched as backslash + char
    prog[escape_jump] = Instr::Jump(base + 1);
    prog.extend([
        Instr::Consume(Consume::BaseChar),
        Instr::Split(base + 4),
        Instr::Consume(Consume::CombiningChar),
        Instr::Jump(base + 1),
        Instr::Split(0),
        Instr::Consume(Consume::CombiningDouble),
        Instr::Jump(base),
    ]);

    prog
}

#[cfg(test)]
pub(crate) fn tokenize(input: &str) -> TokenString {
    tokenize_with_segment_map(input, &mut Default::default())
}

#[cfg(test)]
pub(crate) fn tokenize_with_segment_map(input: &str, segment_map: &mut SegmentMap) -> TokenString {
    Tokens::new(input.as_bytes(), segment_map)
        .collect::<Result<_, _>>()
        .unwrap()
}

/// An `Iterator` that produces the tokens found in a `BufRead`.
#[derive(Debug)]
pub struct Tokens<'s, R> {
    input: R,
    in_buffer: String,
    out_buffer: TokenString,
    index: usize,
    line: usize,
    segment_map: &'s mut SegmentMap,
    re: Program<TokenizerEngine>,
}

impl<'s, R: BufRead> Tokens<'s, R> {
    pub fn new(input: R, segment_map: &'s mut SegmentMap) -> Self {
        let re = matcher(segment_map);
        Tokens {
            input,
            in_buffer: String::new(),
            out_buffer: TokenString::new(),
            index: 0,
            line: 0,
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
            if self.in_buffer.is_empty() {
                return Ok(());
            }
            // perform canonical decomposition on the string
            let mut chars = self.in_buffer.chars().nfd();
            // extract segment boundaries
            let matches = self.re.exec(Default::default(), chars.clone());
            let indices = &matches.first().ok_or(Error::Tokenizing(self.line))?.indices;
            self.line += 1;
            for (start, end) in indices.iter().zip(indices[1..].iter()) {
                // extract the sgement
                let seg = chars.by_ref().take(end - start).collect::<Vec<_>>();
                // get the token corresponding to the segment
                let tok = self.segment_map.get_or_insert(&seg);
                // push the token into the output buffer
                self.out_buffer.push(tok);
            }
        }
        Ok(())
    }

    fn take_out_buffer(&mut self) -> Result<TokenString, Error> {
        self.fill_buffer()?;
        Ok(std::mem::take(&mut self.out_buffer))
    }

    /// An iterator over the lines of the iterator. Lines are ended with `Token::Newline`.
    pub fn lines(self) -> Lines<'s, R> {
        Lines {
            tokens: self,
            fused: false,
        }
    }
}

impl<R: BufRead> Iterator for Tokens<'_, R> {
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Result<Token, Error>> {
        match self.fill_buffer() {
            Err(e) => Some(Err(e)),
            Ok(()) => self.out_buffer.get(self.index).copied().map(|t| {
                self.index += 1;
                Ok(t)
            }),
        }
    }
}

/// An iterator over the lines of a `Tokens` iterator. Generated by the `lines` method on `Tokens`.
pub struct Lines<'s, R: BufRead> {
    tokens: Tokens<'s, R>,
    fused: bool,
}

impl<R: BufRead> Iterator for Lines<'_, R> {
    type Item = Result<TokenString, Error>;

    fn next(&mut self) -> Option<Result<TokenString, Error>> {
        if self.fused {
            None
        } else {
            match self.tokens.take_out_buffer() {
                Ok(mut buf) => {
                    if buf.is_empty() {
                        self.fused = true;
                        return None;
                    }
                    while !buf.ends_with(&[Token::Newline]) {
                        match self.tokens.take_out_buffer() {
                            Ok(next_buf) if next_buf.is_empty() => {
                                self.fused = true;
                                break;
                            }
                            Ok(next_buf) => buf.extend_from_slice(&next_buf),
                            Err(err) => return Some(Err(err)),
                        }
                    }
                    Some(Ok(buf))
                }
                Err(err) => Some(Err(err)),
            }
        }
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

#[cfg(test)]
mod tests {
    use super::*;
    use unicode_normalization::UnicodeNormalization;
    use utf32_lit::utf32;

    #[test]
    fn multiple_possible_segmentations() {
        let line = "antś\n".nfd();
        let segments = SegmentMap::from_iter(utf32!(["an", "nt", "ts"]));
        let prog = matcher(&segments);
        println!("{prog}");

        let matches = prog.exec(Default::default(), line);
        assert_eq!(matches.len(), 1);
        let indices = &*matches.first().unwrap().indices;
        assert_eq!(indices, &[0, 2, 3, 5, 6]);
    }

    #[test]
    fn short_and_long_segments() {
        let line = "shtshth".nfd();
        let segments = SegmentMap::from_iter(utf32!(["sh", "tsh", "th"]));
        let prog = matcher(&segments);
        println!("{prog}");

        let matches = prog.exec(Default::default(), line);
        assert_eq!(matches.len(), 1);
        let indices = &*matches.first().unwrap().indices;
        assert_eq!(indices, &[0, 2, 5, 7]);
    }

    #[test]
    fn combining_double() {
        let line = "t͜s\n".nfd();
        let segments = SegmentMap::from_iter([]);
        let prog = matcher(&segments);
        println!("{prog}");

        let matches = prog.exec(Default::default(), line);
        assert_eq!(matches.len(), 1);
        let indices = &*matches.first().unwrap().indices;
        assert_eq!(indices, &[0, 3, 4]);
    }

    #[test]
    fn backslash() {
        let line = "\\.\n".nfd();
        let segments = SegmentMap::from_iter([]);
        let prog = matcher(&segments);
        println!("{prog}");

        let matches = prog.exec(Default::default(), line);
        assert_eq!(matches.len(), 1);
        let indices = &*matches.first().unwrap().indices;
        assert_eq!(indices, &[0, 2, 3]);
    }

    #[test]
    fn backslash_newline() {
        let line = "\\\n".nfd();
        let segments = SegmentMap::from_iter([]);
        let prog = matcher(&segments);
        println!("{prog}");

        let matches = prog.exec(Default::default(), line);
        assert_eq!(matches.len(), 1);
        let indices = &*matches.first().unwrap().indices;
        assert_eq!(indices, &[0, 2]);
    }

    #[test]
    fn no_newline() {
        let line = "foo".nfd();
        let prog = matcher(&SegmentMap::new());
        println!("{prog}");

        let matches = prog.exec(Default::default(), line);
        assert_eq!(matches.len(), 1);
        let indices = &*matches.first().unwrap().indices;
        assert_eq!(indices, &[0, 1, 2, 3]);
    }

    #[test]
    fn lines() {
        let input = io::Cursor::new("line 1\nline 2\nline 3");
        let mut segment_map = SegmentMap::new();
        let output = Tokens::new(input, &mut segment_map)
            .lines()
            .map(|res| res.unwrap().to_string())
            .collect::<Vec<String>>();
        assert_eq!(output, &["line 1\n", "line 2\n", "line 3"]);
    }
}
