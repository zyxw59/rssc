use std::collections::HashMap;
use std::collections::hash_map::Keys;

use super::Token;

pub type Segment = Vec<char>;

/// A mapping from `Segment` to `Token` values.
#[derive(Clone, Debug)]
pub struct SegmentMap {
    /// A list of `HashMap`s, sorted by key length -- keys of length 1 will be found in map[0],
    /// keys of length 2 will be found in map[1], and so on. Fite me.
    vec: Vec<HashMap<Segment, Token>>,
    /// The largest `Token` value assigned to a `Segment` in the map.
    max_token: Token,
}

impl SegmentMap {
    /// Generates a mapping from a `Vec` of `Segment` values, where the index in the vector is the
    /// offset from `0x80` of the `Token` value (e.g. `Token(0x80)` will be found at index 0,
    /// `Token(0x81)` will be found at index 1, etc.).
    pub fn clone_from_vec(vec: &Vec<Segment>) -> SegmentMap {
        let mut map = SegmentMap {
            vec: Vec::new(),
            max_token: Token(0x7F),
        };
        for (tok, seg) in vec.iter().enumerate() {
            map.pad(seg.len());
            map.vec[seg.len() - 1].insert(seg.clone(), Token::from_index(tok));
        }
        if vec.len() > 0 {
            map.max_token = Token::from_index(vec.len() - 1);
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
        if key.len() == 0 {
            panic!("Segment with zero length");
        }
        if key.len() == 1 && key[0] <= '\x7F' {
            Token::try_from_u8(key[0] as u8)
        } else if key.len() == 2 && key[0] == '\\' && key[1] <= '\x7F' {
            Token::try_from_u8_escaped(key[1] as u8)
        } else {
            None
        }.unwrap_or_else(|| {
            self.pad(key.len());
            let entry = self.vec[key.len() - 1].entry(key);
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
        SegmentMapIter::new(&self.vec)
    }

    /// Pads the internal vector with empty `HashMap`s to a given length.
    fn pad(&mut self, len: usize) {
        while self.vec.len() < len {
            self.vec.push(HashMap::new());
        }
    }
}

/// A struct to iterate over the `Segment`s of a `SegmentMap` in order of decreasing length.
pub struct SegmentMapIter<'a> {
    vec: &'a Vec<HashMap<Segment, Token>>,
    index: usize,
    keys: Option<Keys<'a, Segment, Token>>,
}

impl<'a> SegmentMapIter<'a> {
    fn new(vec: &Vec<HashMap<Segment, Token>>) -> SegmentMapIter {
        let index = vec.len();
        SegmentMapIter {
            vec,
            index,
            keys: None,
        }
    }
}

impl<'a> Iterator for SegmentMapIter<'a> {
    type Item = &'a Segment;

    fn next(&mut self) -> Option<&'a Segment> {
        if self.keys.is_none() {
            self.keys = if self.index > 0 {
                self.index -= 1;
                self.vec.get(self.index).map(|m| m.keys())
            } else {
                None
            };
        }
        self.keys.as_mut().and_then(|ref mut m| m.next())
    }
}
