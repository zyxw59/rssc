use std::collections::hash_map::Keys;
use std::collections::HashMap;

use super::Token;

pub type Segment = Vec<char>;

/// A mapping from `Segment` to `Token` values.
#[derive(Clone, Debug)]
pub struct SegmentMap {
    /// A list of `HashMap`s, sorted by key length -- keys of length 1 will be found in `map[0]`,
    /// keys of length 2 will be found in `maps[1]`, and so on.
    maps: Vec<HashMap<Segment, Token>>,
    /// The largest `Token` value assigned to a `Segment` in the map.
    max_token: Token,
}

impl SegmentMap {
    /// Creates a new, empty `SegmentMap`
    pub fn new() -> SegmentMap {
        SegmentMap {
            maps: Vec::new(),
            max_token: Token(0x7F),
        }
    }

    /// Generates a mapping from a `Vec` of `Segment` values, where the index in the vector is the
    /// offset from `0x80` of the `Token` value (e.g. `Token(0x80)` will be found at index 0,
    /// `Token(0x81)` will be found at index 1, etc.).
    pub fn from_list(list: &[Segment]) -> SegmentMap {
        let mut map = SegmentMap {
            maps: Vec::new(),
            max_token: Token(0x7F),
        };
        for (tok, seg) in list.iter().enumerate() {
            map.pad(seg.len());
            map.maps[seg.len() - 1].insert(seg.clone(), Token::from_index(tok));
        }
        if !list.is_empty() {
            map.max_token = Token::from_index(list.len() - 1);
        }
        map
    }

    /// Retrieves the `Token` associated with a given `Segment`, or adds the `Segment` to the map
    /// at the next available value.
    ///
    /// # Panics
    ///
    /// Panics if `key` is zero-length.
    pub fn get_or_insert(&mut self, key: Segment) -> Token {
        if key.is_empty() {
            panic!("Segment with zero length");
        }
        if key.len() == 1 && key[0] <= '\x7F' {
            Token::try_from_u8(key[0] as u8)
        } else if key.len() == 2 && key[0] == '\\' && key[1] <= '\x7F' {
            Token::try_from_u8_escaped(key[1] as u8)
        } else {
            None
        }
        .unwrap_or_else(|| {
            self.pad(key.len());
            let entry = self.maps[key.len() - 1].entry(key);
            let max_token = self.max_token + 1;
            let token = *entry.or_insert(max_token);
            if token > self.max_token {
                self.max_token = token;
            }
            token
        })
    }

    /// Returns an iterator over the `Segment`s in order of decreasing length.
    pub fn iter(&self) -> SegmentMapIter {
        SegmentMapIter::new(&self.maps)
    }

    /// Pads the internal vector with empty `HashMap`s to a given length.
    fn pad(&mut self, len: usize) {
        if len > self.maps.len() {
            self.maps.resize_with(len, HashMap::new)
        }
    }
}

impl Default for SegmentMap {
    fn default() -> Self {
        SegmentMap::new()
    }
}

/// A struct to iterate over the `Segment`s of a `SegmentMap` in order of decreasing length.
pub struct SegmentMapIter<'a> {
    maps: &'a [HashMap<Segment, Token>],
    keys: Option<Keys<'a, Segment, Token>>,
}

impl<'a> SegmentMapIter<'a> {
    fn new(maps: &[HashMap<Segment, Token>]) -> SegmentMapIter {
        SegmentMapIter { maps, keys: None }
    }
}

impl<'a> Iterator for SegmentMapIter<'a> {
    type Item = &'a Segment;

    fn next(&mut self) -> Option<&'a Segment> {
        loop {
            if let Some(next) = self.keys.as_mut().and_then(Keys::next) {
                return Some(next);
            }
            if let Some((last, rest)) = self.maps.split_last() {
                self.keys = Some(last.keys());
                self.maps = rest;
            } else {
                return None;
            }
        }
    }
}
