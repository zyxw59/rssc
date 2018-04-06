use std::error;
use std::fmt;
use std::io::{self, BufRead};
use std::iter::Peekable;
use std::ops::Add;

use unicode_normalization::UnicodeNormalization;

mod re;
pub mod segment;
mod tokenizer;

use self::segment::{Segment, SegmentMap};

macro_rules! enum_const {
    ( $(#[$attr:meta])* $main:ident ($int:ty) { $($value:pat => $id:ident),* $(,)* } ) => {
        enum _Constants {
            $( $id, )*
        }

        $(#[$attr])*
        pub struct $main($int);

        #[allow(non_upper_case_globals)]
        impl $main {
            $( pub const $id: $main = $main(_Constants::$id as $int); )*

            /// Attempts to generate a value corresponding to the given `u8` if it is one of the
            /// enumerated `u8`s, or `None` otherwise.
            fn try_enum(c: u8) -> Option<$main> {
                match c {
                    $( $value => Some($main::$id), )*
                    _ => None,
                }
            }

            /// Returns whether the given `u8` is one of the enumerated values.
            pub fn is_enumerated(c: u8) -> bool {
                match c {
                    $( $value => true, )*
                    _ => false,
                }
            }
        }
    }
}
enum_const! {
    /// A token for the parser.
    ///
    /// - Characters with special meaning in the rule file format, i.e.
    ///   - Regex control characters (`.*+?()[]|`)
    ///   - Characters with special meaning in patterns (`#$0{}`)
    ///   - Characters which delimit the parser (`=:>_/!|&\n`)
    ///   are mapped to the range `0x00 ... 0x1F`.
    /// - Printable ASCII characters (and backslash-escaped control characters) are mapped to their
    ///   ASCII values.
    /// - All other tokens, including unicode characters, sequences involving combining diacritics, and
    ///   user-defined tokens, are mapped to the range `0x80 ... u16::MAX`
    #[derive(Clone, Copy, Hash, Eq, Ord, PartialEq, PartialOrd)]
    Token(u16) {
        b'.' => Dot,
        b'*' => Star,
        b'+' => Plus,
        b'?' => Question,
        b'(' => OpenParen,
        b')' => CloseParen,
        b'[' => OpenBracket,
        b']' => CloseBracket,
        b'|' => Pipe,
        b'#' => Hash,
        b'$' => Dollar,
        b'0' => Zero,
        b'{' => OpenBrace,
        b'}' => CloseBrace,
        b'=' => Equals,
        b':' => Colon,
        b'>' => Arrow,
        b'_' => Underscore,
        b'/' => Slash,
        b'!' => Exclam,
        b'&' => And,
        b'\n' => Newline,
    }
}

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

    /// Attempts to create a `Token` from the given byte. If it is a control character, this
    /// returns the corresponding value in the range `0x00 ... 0x1F`. Otherwise, if the byte is in
    /// the range `0x20 ... 0x7F`, this returns the numeric value of the byte. If the byte is
    /// outside of that range, then it returns `None`.
    pub fn try_from_u8(c: u8) -> Option<Token> {
        Token::try_enum(c).or_else(|| {
            if c >= 0x20 && c <= 0x7F {
                Some(Token(c as u16))
            } else {
                None
            }
        })
    }

    /// Creates a `Token` from the given byte, escaping control characters. If the byte is a
    /// newline, this returns `Token(0x20)`, i.e. a space. If the byte is any other control
    /// character, it returns a `Token` with the literal value of the byte (_not_ the value
    /// associated with the control character). Otherwise, returns `None`.
    pub fn try_from_u8_escaped(c: u8) -> Option<Token> {
        match c {
            b'\n' => Some(Token(0x20)),
            _ => if Token::is_enumerated(c) {
                Some(Token(c as u16))
            } else {
                None
            },
        }
    }

    /// Returns whether the given `char` is a control character.
    pub fn is_control_char(c: char) -> bool {
        c <= '\x7F' && Token::is_enumerated(c as u8)
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

/// An `Iterator` that produces the tokens found in a `BufRead`.
#[derive(Debug)]
pub struct Tokens<R: BufRead> {
    input: R,
    in_buffer: String,
    out_buffer: Vec<Token>,
    index: usize,
    line: usize,
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
            if chars.len() == 0 {
                return Ok(());
            }
            // extract segment boundaries
            let saves = self.re
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
                .take_while(|t| match *t {
                    Ok(Token::Newline) => false,
                    _ => true,
                })
                // this `.collect()` collects into `Result<Vec<Token>, Error>`
                .collect()
        })
    }
}

/// An error encountered during tokenizing.
#[derive(Debug)]
pub enum Error {
    /// The specified line lacked a valid tokenization.
    Tokenizing(usize),
    /// The specified IO error occurred.
    IO(io::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::Tokenizing(line) => write!(f, "No valid tokenization of line {}", line),
            Error::IO(ref err) => write!(f, "{}", err),
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::Tokenizing(_) => "No valid tokenization",
            Error::IO(ref err) => err.description(),
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            Error::Tokenizing(_) => None,
            Error::IO(ref err) => Some(err),
        }
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::IO(err)
    }
}
