use std::collections::BTreeMap;

use unicode_normalization::UnicodeNormalization;

use super::Token;
use crate::utils::SortByLen;

/// A mapping from `Segment` to `Token` values.
#[derive(Clone, Debug, Default)]
pub struct SegmentMap {
    map: BTreeMap<SortByLen<char>, Token>,
    reverse_map: Vec<Vec<char>>,
}

impl SegmentMap {
    /// Creates a new, empty `SegmentMap`
    pub fn new() -> SegmentMap {
        Default::default()
    }

    /// Retrieves the `Token` associated with a given segment, or adds the segment to the map at
    /// the next available value.
    pub fn get_or_insert(&mut self, segment: &[char]) -> Token {
        let segment = segment.iter().copied().nfd().collect::<Vec<_>>();
        // check for ASCII or escaped control characters
        match *segment {
            [x] if x <= '\x7F' => Token::try_from_u8(x as u8),
            ['\\', x] if x <= '\x7F' => Token::try_from_u8_escaped(x as u8),
            _ => None,
        }
        .unwrap_or_else(|| {
            *self
                .map
                .entry(SortByLen(segment.clone()))
                .or_insert_with(|| {
                    let idx = self.reverse_map.len();
                    self.reverse_map.push(segment);
                    Token::from_index(idx)
                })
        })
    }

    /// Gets the segment corresponding to a given token, if that token is present in the map.
    /// Tokens less than 0x80 (i.e. ASCII characters and control characters) will not be returned.
    pub fn get_segment(&mut self, token: Token) -> Option<&[char]> {
        if token < Token(0x80) {
            // ASCII tokens are not handled here
            None
        } else {
            self.reverse_map
                .get((token.0 - 0x80) as usize)
                .map(|seg| seg.as_slice())
        }
    }

    /// Returns an iterator over the `Segment`s in order of decreasing length.
    pub fn iter(&self) -> impl Iterator<Item = &[char]> {
        self.map.keys().rev().map(|seg| &*seg.0)
    }
}

impl<'s> FromIterator<&'s [char]> for SegmentMap {
    /// Generates a mapping from an iterator of segments, where the first segment is mapped as
    /// `Token(0x80)`, the second segment as `Token(0x81)`, etc.
    ///
    /// Unicode normalization will be applied automatically, and any duplicate segments will be
    /// skipped.
    fn from_iter<T>(list: T) -> SegmentMap
    where
        T: IntoIterator<Item = &'s [char]>,
    {
        let mut map = Self::new();
        for seg in list {
            map.get_or_insert(seg);
        }
        map
    }
}
