//! A [`Category`] is a set of sounds to be used in patterns and replacements.

use std::{
    cmp,
    collections::{BTreeMap, HashMap, HashSet},
};

use crate::{
    re::program::Instr,
    rule::re::{Consume, Engine, Peek},
    token::Token,
};

/// A category name.
pub type Ident = Vec<Token>;
/// An element of a category. `None` represents a gap in the category.
pub type Element = Option<Vec<Token>>;

/// A set of sounds to be used in patterns and replacements
#[derive(Debug)]
pub struct Category {
    /// The elements of the category
    elements: Vec<Element>,
    /// A map from elements to the indices of those elements. Note that an element might correspond
    /// to multiple indices.
    indices: BTreeMap<SortByLen<Token>, Vec<usize>>,
    /// A map from single-token elements to the indices of those elements.
    /// Only populated if none of those elements are repeated.
    single_map: Option<HashMap<Token, usize>>,
    /// The set of single-token elements.
    single_set: HashSet<Token>,
    /// The name of the category
    name: Ident,
}

impl Category {
    /// Constructs a category from a name and a vector of elements
    pub fn new(name: Ident, elements: Vec<Element>) -> Category {
        let n = elements.len();
        let mut indices = BTreeMap::<_, Vec<usize>>::new();
        let mut single_set = HashSet::with_capacity(n);
        let mut single_map = HashMap::with_capacity(n);
        let mut can_use_single_map = true;
        for (i, el) in elements.iter().enumerate() {
            if let Some(el) = el {
                if let &[tok] = &el[..] {
                    if single_set.insert(tok) {
                        // first time seeing this element
                        single_map.insert(tok, i);
                    } else {
                        // we've seen this element before
                        can_use_single_map = false;
                    }
                }
                indices.entry(SortByLen(el.clone())).or_default().push(i);
            }
        }

        let single_map = if can_use_single_map {
            Some(single_map)
        } else {
            None
        };
        Category {
            elements,
            indices,
            single_map,
            single_set,
            name,
        }
    }

    pub fn capturing_matcher(&self, instruction_list: &mut Vec<Instr<Token, Engine>>, slot: usize) {
        let jump_instr = instruction_list.len();
        // the actual destination of this jump will be filled in at the end of the function
        instruction_list.push(Instr::Jump(0));
        // match longer elements first
        for (el, indices) in self.indices.iter().rev() {
            if self.single_map.is_some() && el.0.len() == 1 {
                // single-length elements to be handled with a single Consume::Category instruction
                // we can break here because the elements are already sorted by length
                break;
            }
            if let Some((&last, rest)) = indices.split_last() {
                // 1 instruction for the split to the next element
                // 3 instructions for each index except the last one
                //   Split(to next index);
                //   Category { slot, index };
                //   Jump(to element match)
                // plus 1 for the last index:
                //   Category { slot, index };
                let element_match_start = instruction_list.len() + 3 * rest.len() + 2;
                // element match is 1 instruction per token, plus 1 for the jump to after the
                // category
                let next_element_start = element_match_start + el.0.len() + 1;
                instruction_list.push(Instr::Split(next_element_start));
                for &index in rest {
                    let pc = instruction_list.len();
                    instruction_list.extend([
                        Instr::Split(pc + 3),
                        Instr::Peek(Peek::Category { slot, index }),
                        Instr::Jump(element_match_start),
                    ]);
                }
                instruction_list.push(Instr::Peek(Peek::Category { slot, index: last }));
                instruction_list
                    .extend(el.0.iter().map(|&tok| Instr::Consume(Consume::Token(tok))));
                instruction_list.push(Instr::Jump(jump_instr));
                debug_assert_eq!(instruction_list.len(), next_element_start);
            }
        }
        // handle single-length elements with a `Consume::Category`, if possible
        if let Some(map) = self.single_map.clone() {
            instruction_list.extend([
                Instr::Consume(Consume::Category { slot, map }),
                Instr::Jump(jump_instr),
            ]);
        }
        // if we weren't able to use the map here, the last element will have a split pointing to
        // this point in the program. since there are no more elements to match, we should put a
        // reject here, ending that thread.
        instruction_list.push(Instr::Reject);
        let after_category_match = instruction_list.len();
        instruction_list[jump_instr] = Instr::Jump(after_category_match);
    }

    pub fn non_capturing_matcher(&self, instruction_list: &mut Vec<Instr<Token, Engine>>) {
        let jump_instr = instruction_list.len();
        // the actual destination of this jump will be filled in at the end of the function
        instruction_list.push(Instr::Jump(0));
        // match longer elements first
        for (el, _indices) in self.indices.iter().rev() {
            if el.0.len() == 1 {
                // single-length elements to be handled with a single Consume::Set instruction
                // we can break here because the elements are already sorted by length
                break;
            }
            // element match is 1 instruction per token, plus 1 for the jump to after the category
            let next_element_start = instruction_list.len() + el.0.len() + 1;
            instruction_list.push(Instr::Split(next_element_start));
            instruction_list.extend(el.0.iter().map(|&tok| Instr::Consume(Consume::Token(tok))));
            instruction_list.push(Instr::Jump(jump_instr));
            debug_assert_eq!(instruction_list.len(), next_element_start);
        }
        // handle single-length elements with a `Consume::Set`
        instruction_list.extend([
            Instr::Consume(Consume::Set(self.single_set.clone())),
            Instr::Jump(jump_instr),
        ]);
        let after_category_match = instruction_list.len();
        instruction_list[jump_instr] = Instr::Jump(after_category_match);
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SortByLen<T>(Vec<T>);

impl<T: cmp::Ord> cmp::Ord for SortByLen<T> {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.0
            .len()
            .cmp(&other.0.len())
            .then_with(|| self.0.cmp(&other.0))
    }
}

impl<T: cmp::Ord> cmp::PartialOrd for SortByLen<T> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}
