use std::cmp;
use std::fmt;
use std::hash;

/// A single token for matching
pub trait Token: cmp::Eq + fmt::Debug + hash::Hash {
    /// Returns whether the `Token` should be considered a word character.
    fn is_word(&self) -> bool;
}

impl Token for char {
    /// Returns `false` if the character is whitespace, `true` otherwise.
    fn is_word(&self) -> bool {
        !self.is_whitespace()
    }
}

