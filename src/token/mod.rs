use std::fmt;
use std::io::{self,BufRead};
use std::ops::Add;

use unicode_normalization::UnicodeNormalization;

mod re;
pub mod segment;
mod tokenizer;

use self::segment::{Segment, SegmentMap};

//use segment::Segment;

/// A token for the parser.
/// 
/// - Characters with special meaning in the rule file format, i.e.
///   - Regex control characters (`.*+?()[]|`)
///   - Characters with special meaning in patterns (`#$0{}`)
///   - Characters which delimit the parser (`>_/!|&\n`)
///   are mapped to the range `0x00 ... 0x1F`.
/// - Printable ASCII characters (and backslash-escaped control characters) are mapped to their
///   ASCII values.
/// - All other tokens, including unicode characters, sequences involving combining diacritics, and
///   user-defined tokens, are mapped to the range `0x80 ... u16::MAX`
#[derive(Clone, Copy, Hash, Eq, Ord, PartialEq, PartialOrd)]
pub struct Token(u16);

impl Token {
    /// Creates a `Token` with the given offset from `0x80`.
    ///
    /// # Panics
    ///
    /// Panics if `index + 0x80` is greater than `u16::MAX`
    pub fn from_index(index: usize) -> Token {
        if index > (::std::u16::MAX as usize) - 0x80 {
            panic!("index {} out of range for legal `Token` values", index);
        }
        Token((index as u16) + 0x80)
    }

    /// Creates a `Token` from the given byte. If it is a control character, returns the
    /// corresponding value in the range `0x00 ... 0x1F`. Otherwise, returns the numeric value of
    /// the byte.
    pub fn from_u8(c: u8) -> Token {
        match ControlChar::try_from_char(c as char) {
            Some(c) => Token(c as u16),
            None => Token(c as u16),
        }
    }

    /// Creates a `Token` from the given byte, always returning the numeric value of the byte.
    pub fn from_u8_escaped(c: u8) -> Token {
        Token(c as u16)
    }
}

impl Add<u16> for Token {
    type Output = Token;

    fn add(self, rhs: u16) -> Token {
        Token(self.0 + rhs)
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Token({:#X})", self.0)
    }
}

pub enum ControlChar {
    Dot,
    Star,
    Plus,
    Question,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    Pipe,
    Hash,
    Dollar,
    Zero,
    OpenBrace,
    CloseBrace,
    Arrow,
    Underscore,
    Slash,
    Exclam,
    And,
    Newline,
}

impl ControlChar {
    pub fn try_from_char(c: char) -> Option<ControlChar> {
        use self::ControlChar::*;
        match c {
            '.' => Some(Dot),
            '*' => Some(Star),
            '+' => Some(Plus),
            '?' => Some(Question),
            '(' => Some(OpenParen),
            ')' => Some(CloseParen),
            '[' => Some(OpenBracket),
            ']' => Some(CloseBracket),
            '|' => Some(Pipe),
            '#' => Some(Hash),
            '$' => Some(Dollar),
            '0' => Some(Zero),
            '{' => Some(OpenBrace),
            '}' => Some(CloseBrace),
            '>' => Some(Arrow),
            '_' => Some(Underscore),
            '/' => Some(Slash),
            '!' => Some(Exclam),
            '&' => Some(And),
            '\n' => Some(Newline),
            _ => None,
        }
    }

    pub fn is_control_char(c: char) -> bool {
        c <= '\x7f' && ControlChar::try_from_char(c).is_some()
    }
}

/// An `Iterator` that produces the tokens found in a `BufRead`.
#[derive(Debug)]
pub struct Tokens<R: BufRead> {
    input: R,
    in_buffer: String,
    out_buffer: Vec<Token>,
    index: usize,
    token_map: Vec<Segment>,
    segment_map: SegmentMap,
    re: re::Program,
}

impl<R: BufRead> Tokens<R> {
    pub fn new(input: R, segment_map: SegmentMap) -> Tokens<R> {
        let re = tokenizer::matcher(&segment_map);
        Tokens {
            input,
            in_buffer: String::new(),
            out_buffer: Vec::new(),
            index: 0,
            token_map: Vec::new(),
            segment_map,
            re,
        }
    }

    /// Fills the internal incoming and outgoing buffers
    ///
    /// # Panics
    ///
    /// Panics if a line cannot be tokenized.
    /// TODO: This should return an error, rather than panicking.
    fn fill_buffer(&mut self) -> io::Result<()> {
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
            if chars.len() == 0 {
                return Ok(());
            }
            // extract segment boundaries
            let saves = self.re.exec(chars.iter());
            // TODO: return an error here, don't just panic
             let saves = saves.expect("no valid tokenization of line");
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
            self.out_buffer.push(Token::from_u8(b'\n'));
        }
        Ok(())
    }
}

impl<R: BufRead> Iterator for Tokens<R> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        if self.fill_buffer().is_err() { return None; }
        match self.out_buffer.get(self.index) {
            Some(t) => {self.index += 1; Some(*t)},
            None => None,
        }
    }
}
