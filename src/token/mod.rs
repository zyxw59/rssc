use std::fmt;
use std::ops::Add;

pub mod segment;

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
