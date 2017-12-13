use std::cmp;
use std::collections::{BTreeSet,HashMap,HashSet};
use std::collections::hash_map::Entry;

use segment::Segment;
use re::ast::Regex;

/// A set of sounds to be used in patterns and replacements
#[derive(Debug)]
pub struct Category {
    /// The elements of the category
    elements: Vec<Vec<Segment>>,
    /// A regular expression matching the category.
    pattern: Regex<Segment>,
    /// A map from elements to the indices of those elements. Note that an element might correspond
    /// to multiple indices.
    indices: HashMap<Vec<Segment>, Vec<usize>>,
    /// The name of the category
    name: String,
}

impl Category {
    /// Constructs a category from a name and a vector of elements
    pub fn new(name: String, elements: Vec<String>) -> Category {
        let n = elements.len();
        // if all elements are length 1, we can use a `Set` instruction to match the category,
        // otherwise we must use an `Alternate` instruction.
        let mut can_use_set = true;
        // rename input elements vector
        let els = elements;
        // new element vector
        let mut elements = Vec::with_capacity(n);
        // indices map
        let mut indices = HashMap::with_capacity(n);
        // to hold elements, sorted by length
        let mut sorted = BTreeSet::new();
        // to hold a set of elements if they are all one segment
        let mut set = HashSet::with_capacity(n);
        // iterate over elements
        for el in els {
            let mut e = Vec::new();
            // Don't parse "0" as a segment (it represents the empty string)
            if el != "0" {
                e = Segment::parse_string(el);
                if can_use_set && e.len() == 1 {
                    set.insert(e[0].clone());
                } else {
                    can_use_set = false;
                }
                // current index
                let i = elements.len();
                match indices.entry(e.clone()) {
                    Entry::Vacant(entry) => {
                        // this is the first instance of this element
                        // store the index in this entry
                        entry.insert(vec![i]);
                        // sort this element by length
                        sorted.insert(SortKey {
                            key: !e.len(),
                            value: i,
                        });
                    },
                    Entry::Occupied(mut entry) => {
                        // store the index in this entry
                        entry.get_mut().push(i);
                        // no need to put it in the sorted list, because we've already seen it (and
                        // a|b|a matches the same things as a|b)
                    },
                }
            }
            // push the element
            elements.push(e);
        }
        let pattern = if can_use_set {
            Regex::Set(set)
        } else {
            Regex::Alternate(
                sorted.iter().map(|x| Regex::Literal(elements[x.value].clone())).collect())
        };
        Category {
            elements,
            pattern,
            indices,
            name,
        }
    }
}

/// A struct for sorting arbitrary data by an arbitrary key
#[derive(Debug,Clone,Copy)]
struct SortKey<T> {
    pub key: usize,
    pub value: T,
}

impl<T> cmp::PartialEq for SortKey<T> {
    fn eq(&self, other: &SortKey<T>) -> bool {
        self.key == other.key
    }
}

impl<T> cmp::Eq for SortKey<T> {}

impl<T> cmp::Ord for SortKey<T> {
    fn cmp(&self, other: &SortKey<T>) -> cmp::Ordering {
        self.key.cmp(&other.key)
    }
}

impl<T> cmp::PartialOrd for SortKey<T> {
    fn partial_cmp(&self, other: &SortKey<T>) -> Option<cmp::Ordering> {
        Some(self.key.cmp(&other.key))
    }
}
