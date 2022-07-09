//! Irregex engine for matching a [`Pattern`](super::Pattern)

use std::collections::{BTreeMap, HashMap, HashSet};

use crate::token::Token;

#[derive(Clone, Debug, Default, Hash, Eq, PartialEq)]
pub struct Engine {
    /// Whether the previous token was a whitespace token.
    is_whitespace: bool,
    /// Matched category indices.
    pub category_indices: CategoryIndices,
    /// Start of the section to replace.
    pub replace_start: Option<usize>,
    /// End of the section to replace.
    pub replace_end: Option<usize>,
}

impl Engine {
    /// Returns the start and end indices of the replacement section if both are set.
    pub fn replace_indices(&self) -> Option<(usize, usize)> {
        Some((self.replace_start?, self.replace_end?))
    }
}

impl crate::re::Engine for Engine {
    type Token = Token;
    type Consume = Consume;
    type Peek = Peek;

    fn consume(&mut self, args: &Consume, _index: usize, token: &Self::Token) -> bool {
        self.is_whitespace = token.is_whitespace();
        match args {
            Consume::Any => true,
            Consume::Token(expect) => token == expect,
            Consume::Set(set) => set.contains(token),
            Consume::Category { slot, map } => {
                if let Some(index) = map.get(token) {
                    self.category_indices.check_insert(*slot, *index)
                } else {
                    // didn't match the category
                    false
                }
            }
        }
    }

    fn peek(&mut self, args: &Peek, index: usize, token: Option<&Self::Token>) -> bool {
        match *args {
            Peek::ReplaceStart => self.replace_start.replace(index).is_none(),
            Peek::ReplaceEnd => self.replace_end.replace(index).is_none(),
            Peek::WordBoundary => {
                // end of string is considered whitespace
                self.is_whitespace ^ token.map_or(true, |&tok| tok.is_whitespace())
            }
            Peek::Category { slot, index } => self.category_indices.check_insert(slot, index),
        }
    }
}

#[derive(Clone, Debug, Default, Hash, Eq, PartialEq)]
pub struct CategoryIndices(pub BTreeMap<usize, usize>);

impl CategoryIndices {
    /// Inserts the specified index at the specified slot. Returns false if the slot was occupied
    /// and does not match the specified index.
    fn check_insert(&mut self, slot: usize, index: usize) -> bool {
        if let Some(prev) = self.0.insert(slot, index) {
            // accept iff the previously matched index was the same as this one
            prev == index
        } else {
            // this slot hasn't been set, so this match is ok
            true
        }
    }
}

#[derive(Debug)]
pub enum Consume {
    /// Matches any token
    Any,
    /// Matches the specified token
    Token(Token),
    /// Matches one of a set of tokens
    Set(HashSet<Token>),
    /// Matches an element of a category, saving the matched index in the specified category slot.
    ///
    /// This can only be used for categories where:
    /// - Every (non-null) element is a single token (there is no way to consume multiple tokens at
    ///   once), and
    /// - There are no repeated elements (if a repeated element is matched, there are two possible
    ///   indices to store
    Category {
        slot: usize,
        map: HashMap<Token, usize>,
    },
}

#[derive(Debug)]
pub enum Peek {
    /// Saves the start of the section to be replaced. Rejects the match if it has already been
    /// set.
    ReplaceStart,
    /// Saves the end of the section to be replaced. Rejects the match if it has already been set.
    ReplaceEnd,
    /// Matches if the current position is at a word boundary.
    WordBoundary,
    /// Saves the specified index in the specified category slot.
    Category { slot: usize, index: usize },
}
