//! The [`Token`] type represents a grapheme or control character.
use std::fmt;
use std::ops::Add;

mod escape;
pub mod segment;
pub mod string;
pub mod tokenizer;

use escape::{Escape, EscapeArgs};
pub use segment::SegmentMap;
pub use string::{TokenStr, TokenString};
pub use tokenizer::Tokens;

macro_rules! enum_const {
    (
        $(#[$attr:meta])*
        $main:ident ($vis:vis $int:ty) {
            $($value:literal => $id:ident),* $(,)*
        }
    ) => {
        enum _Constants {
            $( $id, )*
        }

        $(#[$attr])*
            pub struct $main($vis $int);

        #[allow(non_upper_case_globals)]
        impl $main {
            $( pub const $id: $main = $main(_Constants::$id as $int); )*

                /// Attempts to generate a value corresponding to the given `u8` if it is one of the
                /// enumerated `u8`s, or `None` otherwise.
                fn try_enum(c: u8) -> Option<Self> {
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

            /// Returns the value as a `u8` if it is an enumerated value, or the original value
            /// otherwise.
            fn as_enumerated(self) -> Result<u8, Self> {
                match self {
                    $( $main::$id => Ok($value), )*
                        _ => Err(self),
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
    ///   - Characters with special meaning in patterns (`#0{}`)
    ///   - Characters which delimit the parser (`=:>_/!|&\n\t `)
    ///   are mapped to the range `0x00 ..= 0x1F`.
    /// - Printable ASCII characters (and backslash-escaped control characters) are mapped to their
    ///   ASCII values.
    /// - All other tokens, including unicode characters, sequences involving combining diacritics, and
    ///   user-defined tokens, are mapped to the range `0x80 ..= u16::MAX`
    #[derive(Clone, Copy, Hash, Eq, Ord, PartialEq, PartialOrd)]
    Token(pub(crate) u16) {
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
        b'\t' => Tab,
        b' ' => Space,
    }
}

impl Token {
    /// Creates a `Token` with the given offset from `0x80`.
    ///
    /// # Panics
    ///
    /// Panics if `index + 0x80` is greater than `u16::MAX`
    pub fn from_index(index: usize) -> Token {
        if index > (u16::MAX as usize) - 0x80 {
            panic!("index {} out of range for legal `Token` values", index);
        }
        Token((index as u16) + 0x80)
    }

    /// Attempts to create a `Token` from the given byte. If it is a control character, this
    /// returns the corresponding value in the range `0x00 ..= 0x1F`. Otherwise, if the byte is in
    /// the range `0x20 ..= 0x7F`, this returns the numeric value of the byte. If the byte is
    /// outside of that range, then it returns `None`.
    pub fn try_from_u8(c: u8) -> Option<Token> {
        Token::try_enum(c).or(if (0x20..=0x7f).contains(&c) {
            Some(Token(c as u16))
        } else {
            None
        })
    }

    /// Creates a `Token` from the given byte, escaping control characters. If the byte is a
    /// newline, this returns `Token(0x20)`, i.e. a space. If the byte is any other control
    /// character, it returns a `Token` with the literal value of the byte (_not_ the value
    /// associated with the control character). Otherwise, returns `None`.
    pub fn try_from_u8_escaped(c: u8) -> Option<Token> {
        match c {
            b'\n' => Some(Token::Space),
            _ => {
                if Token::is_enumerated(c) {
                    Some(Token(c as u16))
                } else {
                    None
                }
            }
        }
    }

    /// Returns whether the given `char` is a control character.
    pub fn is_control_char(c: char) -> bool {
        c <= '\x7F' && Token::is_enumerated(c as u8)
    }

    pub fn is_control_token(self) -> bool {
        self < Token(0x20)
    }

    /// Returns whether the token is `' '`, `'\n'`, or `'\t'`.
    pub fn is_whitespace(self) -> bool {
        matches!(self, Token::Newline | Token::Tab | Token::Space)
    }

    /// Returns whether the token is a digit.
    pub fn is_digit(self) -> bool {
        matches!(self, Token::Zero | Token(0x30..=0x39))
    }

    /// Returns the value of the token as a decimal digit, or `None` if it is not a digit.
    pub fn digit_value(self) -> Option<u8> {
        match self {
            Token::Zero => Some(0),
            Token(x @ 0x30..=0x39) => Some(x as u8 - 0x30),
            _ => None,
        }
    }

    fn escape(&self, args: EscapeArgs) -> Escape {
        const SINGLE_QUOTE: u16 = b'\'' as u16;
        const DOUBLE_QUOTE: u16 = b'\"' as u16;
        if let Ok(enumerated) = self.as_enumerated() {
            match enumerated {
                b'\n' if args.escape_newline => Escape::backslash('n'),
                b'\t' if args.escape_tab => Escape::backslash('t'),
                _ => Escape::char(enumerated as char),
            }
        } else {
            match self.0 {
                // escaped control character
                x @ 0..=0x7F if Self::is_enumerated(x as u8) => Escape::backslash(x as u8 as char),
                SINGLE_QUOTE if args.escape_single_quote => Escape::backslash('\''),
                DOUBLE_QUOTE if args.escape_double_quote => Escape::backslash('\"'),
                // regular ascii character
                x @ 0x20..=0x7F => Escape::char(x as u8 as char),
                x => Escape::numeric(x),
            }
        }
    }
}

impl Add<u16> for Token {
    type Output = Token;

    fn add(self, rhs: u16) -> Token {
        Token(self.0 + rhs)
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.escape(EscapeArgs::NONE), f)
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "'{}'",
            self.escape(EscapeArgs {
                escape_double_quote: false,
                ..EscapeArgs::ALL
            }),
        )
    }
}
