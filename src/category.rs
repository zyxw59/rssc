use std::cmp;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::collections::hash_map::Entry;

use ast::Ident;
use re::ast::Regex;
use token::Token;

/// A set of sounds to be used in patterns and replacements
#[derive(Debug)]
pub struct Category {
    /// The elements of the category
    elements: Vec<Element>,
    /// A regular expression matching the category.
    pattern: Regex<Token>,
    /// A map from elements to the indices of those elements. Note that an element might correspond
    /// to multiple indices.
    indices: HashMap<Vec<Token>, Vec<usize>>,
    /// The name of the category
    name: Ident,
}

impl Category {
    /// Constructs a category from a name and a vector of elements
    pub fn new(name: Ident, elements: Vec<Element>) -> Category {
        let n = elements.len();
        // if all elements are empty or single tokens, we can use a `Set` instruction to match the
        // category, otherwise we must use an `Alternate` instruction.
        let mut can_use_set = true;
        // indices map
        let mut indices = HashMap::with_capacity(n);
        // to hold elements, sorted by length
        let mut sorted = BTreeSet::new();
        // to hold a set of elements if they are all one segment
        let mut set = HashSet::with_capacity(n);
        // iterate over elements
        for el in &elements {
            if let &Element::String(ref el) = el {
                if can_use_set && el.len() == 1 {
                    set.insert(el[0]);
                } else {
                    can_use_set = false;
                }
                let i = elements.len();
                match indices.entry(el.clone()) {
                    Entry::Vacant(entry) => {
                        // this is the first instance of this element
                        // store the index in this entry
                        entry.insert(vec![i]);
                        // sort this element by length
                        sorted.insert(SortKey {
                            key: !el.len(),
                            value: i,
                        });
                    }
                    Entry::Occupied(mut entry) => {
                        // store the index in this entry
                        entry.get_mut().push(i);
                        // no need to put it in the sorted list, because we've already seen it (and
                        // a|b|a matches the same things as a|b)
                    }
                }
            }
        }
        let pattern = if can_use_set {
            Regex::Set(set)
        } else {
            Regex::Alternate(
                sorted
                    .iter()
                    .filter_map(|x| elements[x.value].string_or_none())
                    .map(Clone::clone)
                    .map(Regex::Literal)
                    .collect(),
            )
        };
        Category {
            elements,
            pattern,
            indices,
            name,
        }
    }
}

/// An element of a category.
#[derive(Clone, Debug)]
pub enum Element {
    /// A null element.
    Zero,
    /// A string of tokens.
    String(Vec<Token>),
}

impl Element {
    fn string_or_none(&self) -> Option<&Vec<Token>> {
        match self {
            &Element::Zero => None,
            &Element::String(ref v) => Some(v),
        }
    }
}

/// A struct for sorting arbitrary data by an arbitrary key
#[derive(Debug, Clone, Copy)]
struct SortKey<T> {
    pub key: usize,
    pub value: T,
}

impl<T, U> cmp::PartialEq<SortKey<U>> for SortKey<T> {
    fn eq(&self, other: &SortKey<U>) -> bool {
        self.key == other.key
    }
}

impl<T> cmp::Eq for SortKey<T> {}

impl<T, U> cmp::PartialOrd<SortKey<U>> for SortKey<T> {
    fn partial_cmp(&self, other: &SortKey<U>) -> Option<cmp::Ordering> {
        Some(self.key.cmp(&other.key))
    }
}

impl<T> cmp::Ord for SortKey<T> {
    fn cmp(&self, other: &SortKey<T>) -> cmp::Ordering {
        self.key.cmp(&other.key)
    }
}
