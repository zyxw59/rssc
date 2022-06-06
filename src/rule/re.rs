//! Irregex engine for matching a [`Pattern`](super::Pattern)

use std::collections::{BTreeMap, HashMap, HashSet};

use crate::re::engine;
use crate::token::Token;

#[derive(Clone, Debug, Hash)]
pub struct Engine {
    /// Whether the previous token was a whitespace token.
    is_whitespace: bool,
    /// Matched category indices.
    category_indices: CategoryIndices,
}

impl engine::Engine<Token> for Engine {
    /// Any category indices which have already been matched by other parts of this rule.
    type Init = CategoryIndices;
    type Consume = Consume;
    type Peek = Peek;

    fn initialize(category_indices: &Self::Init) -> Self {
        Engine {
            // beginning of string is considered whitespace
            is_whitespace: true,
            category_indices: category_indices.clone(),
        }
    }

    fn consume(&mut self, args: &Consume, _index: usize, token: &Token) -> bool {
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

    fn peek(&mut self, args: &Peek, _index: usize, token: Option<&Token>) -> bool {
        match *args {
            Peek::WordBoundary => {
                // end of string is considered whitespace
                self.is_whitespace ^ token.map_or(true, |&tok| tok.is_whitespace())
            }
            Peek::Category { slot, index } => self.category_indices.check_insert(slot, index),
        }
    }
}

#[derive(Clone, Debug, Hash)]
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

pub enum Peek {
    /// Matches if the current position is at a word boundary.
    WordBoundary,
    /// Saves the specified index in the specified category slot.
    Category { slot: usize, index: usize },
}
