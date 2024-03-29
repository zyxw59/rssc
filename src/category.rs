//! A [`Category`] is a set of sounds to be used in patterns and replacements.

use std::collections::{BTreeMap, HashMap, HashSet};

use irregex::{Instr, Program};

use crate::{
    rule::re::{Consume, Engine, Peek},
    token::{Token, TokenStr, TokenString},
};

/// A category name.
pub type Ident = TokenString;
/// An element of a category. `None` represents a gap in the category.
pub type Element = Option<TokenString>;
/// A map from names to categories.
pub type Categories = HashMap<Ident, Category>;

/// A set of sounds to be used in patterns and replacements
#[derive(Debug)]
pub struct Category {
    /// The elements of the category
    elements: Vec<Element>,
    /// A map from elements to the indices of those elements. Note that an element might correspond
    /// to multiple indices.
    indices: BTreeMap<TokenString, Vec<usize>>,
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
        let mut indices = BTreeMap::<TokenString, Vec<usize>>::new();
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
                indices.entry(el.clone()).or_default().push(i);
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

    pub fn capturing_matcher(
        &self,
        instruction_list: &mut Program<Engine>,
        slot: u8,
        reverse: bool,
    ) {
        // skip over the jump instruction
        let start_of_category = instruction_list.len() + 2;
        instruction_list.push(Instr::Jump(start_of_category));
        let jump_instr = instruction_list.len();
        // the actual destination of this jump will be filled in at the end of the function
        instruction_list.push(Instr::Jump(0));
        // match longer elements first
        for (el, indices) in self.indices.iter().rev() {
            if self.single_map.is_some() && el.len() == 1 {
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
                let next_element_start = element_match_start + el.len() + 1;
                instruction_list.push(Instr::Split(next_element_start));
                for &index in rest {
                    let pc = instruction_list.len();
                    instruction_list.extend([
                        Instr::Split(pc + 3),
                        Instr::Peek(Peek::Category { slot, index }),
                        Instr::Jump(element_match_start),
                    ]);
                }
                instruction_list.peek(Peek::Category { slot, index: last });
                match_string(el, instruction_list, reverse);
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

    pub fn non_capturing_matcher(&self, instruction_list: &mut Program<Engine>, reverse: bool) {
        // skip over the jump instruction
        let start_of_category = instruction_list.len() + 2;
        instruction_list.push(Instr::Jump(start_of_category));
        let jump_instr = instruction_list.len();
        // the actual destination of this jump will be filled in at the end of the function
        instruction_list.push(Instr::Jump(0));
        // match longer elements first
        for (el, _indices) in self.indices.iter().rev() {
            if el.len() == 1 {
                // single-length elements to be handled with a single Consume::Set instruction
                // we can break here because the elements are already sorted by length
                break;
            }
            // element match is 1 instruction per token, plus 1 for the split, and 1 for jump to
            // after the category
            let next_element_start = instruction_list.len() + el.len() + 2;
            instruction_list.push(Instr::Split(next_element_start));
            match_string(el, instruction_list, reverse);
            instruction_list.push(Instr::Jump(jump_instr));
            debug_assert_eq!(
                instruction_list.len(),
                next_element_start,
                "wrong number of instructions in instruction list: \
                expected {next_element_start}, found {}.\n\
                Full listing of instructions:\n{instruction_list}",
                instruction_list.len(),
            );
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

fn match_string(string: &TokenStr, program: &mut Program<Engine>, reverse: bool) {
    let it = string
        .iter()
        .map(|&tok| Instr::Consume(Consume::Token(tok)));
    if reverse {
        program.extend(it.rev());
    } else {
        program.extend(it);
    }
}

#[cfg(test)]
mod tests {
    use irregex::Program;

    use crate::token::tokenizer::tokenize;

    use super::Category;

    #[test]
    fn simple() {
        let elements = vec![
            Some(tokenize("m")),
            Some(tokenize("n")),
            Some(tokenize("gn")),
            Some(tokenize("ng")),
        ];
        let name = tokenize("N");
        let category = Category::new(name, elements);
        let mut program = Program::floating_start();
        category.non_capturing_matcher(&mut program, false);
        println!("{program}");
        let test_string = tokenize("man");
        assert_eq!(
            program.exec(Default::default(), test_string.clone()).len(),
            2
        );

        let mut program = Program::floating_start();
        category.capturing_matcher(&mut program, 0, false);
        println!("{program}");
        let matches = program.exec(Default::default(), test_string);
        let indices = matches
            .into_iter()
            .map(|engine| engine.category_indices.0.into_values().collect::<Vec<_>>())
            .collect::<Vec<_>>();

        assert_eq!(indices, &[&[0], &[1]]);
    }

    #[test]
    fn ambiguous_match() {
        let elements = vec![
            Some(tokenize("m")),
            Some(tokenize("n")),
            Some(tokenize("gn")),
            Some(tokenize("ng")),
        ];
        let name = tokenize("N");
        let category = Category::new(name, elements);

        let mut program = Program::new();
        category.capturing_matcher(&mut program, 0, false);
        category.capturing_matcher(&mut program, 1, false);
        println!("{program}");
        let test_string = tokenize("ngn");
        let matches = program.exec(Default::default(), test_string);
        let indices = matches
            .into_iter()
            .map(|engine| engine.category_indices.0.into_values().collect::<Vec<_>>())
            .collect::<Vec<_>>();
        assert_eq!(indices, &[&[3, 1], &[1, 2]]);

        let mut program = Program::floating_start();
        category.capturing_matcher(&mut program, 0, false);
        println!("{program}");
        let test_string = tokenize("ng");
        let matches = program.exec(Default::default(), test_string);
        let indices = matches
            .into_iter()
            .map(|engine| engine.category_indices.0.into_values().collect::<Vec<_>>())
            .collect::<Vec<_>>();
        assert_eq!(indices, &[&[3], &[1]]);

        let mut program = Program::floating_start();
        category.capturing_matcher(&mut program, 0, false);
        println!("{program}");
        let test_string = tokenize("gn");
        let matches = program.exec(Default::default(), test_string);
        let indices = matches
            .into_iter()
            .map(|engine| engine.category_indices.0.into_values().collect::<Vec<_>>())
            .collect::<Vec<_>>();
        assert_eq!(indices, &[&[2], &[1]]);
    }

    #[test]
    fn ambiguous_match_2() {
        let elements = vec![
            Some(tokenize("m")),
            Some(tokenize("n")),
            Some(tokenize("n")),
            Some(tokenize("ng")),
        ];
        let name = tokenize("N");
        let category = Category::new(name, elements);
        let mut program = Program::new();

        category.capturing_matcher(&mut program, 0, false);
        println!("{program}");
        let test_string = tokenize("ng");
        let matches = program.exec(Default::default(), test_string);
        let indices = matches
            .into_iter()
            .map(|engine| engine.category_indices.0.into_values().collect::<Vec<_>>())
            .collect::<Vec<_>>();
        assert_eq!(indices, &[&[3], &[1], &[2]]);
    }
}
