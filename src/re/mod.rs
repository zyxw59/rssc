//! Pattern matching based on regular expressions (but with a bit more power).
//pub mod ast;
pub mod engine;
pub mod program;
pub mod token;

#[cfg(test)]
mod tests {
    use super::{
        //ast,
        engine::{Check, Engine},
        program,
        token::Token,
    };

    #[derive(Clone, Debug)]
    pub struct TestEngine {
        saves: Vec<Option<usize>>,
        is_word: bool,
    }

    impl<T: Token> Engine<T> for TestEngine {
        /// Number of save slots.
        type Init = usize;
        type Consumer = TestConsumer<T>;
        type Peeker = TestPeeker;

        fn initialize(num_slots: Self::Init) -> Self {
            TestEngine {
                saves: vec![None; num_slots],
                is_word: false,
            }
        }
    }

    pub enum TestConsumer<T> {
        Any,
        Token(T),
    }

    impl<'t, T: Token> Check<TestEngine, &'t T> for TestConsumer<T> {
        fn check(&self, engine: &mut TestEngine, _index: usize, token: &'t T) -> bool {
            engine.is_word = token.is_word();
            match self {
                Self::Any => true,
                Self::Token(expected) => expected == token,
            }
        }
    }

    pub enum TestPeeker {
        WordBoundary,
        Save(usize),
    }

    impl<'t, T: Token> Check<TestEngine, Option<&'t T>> for TestPeeker {
        fn check(&self, engine: &mut TestEngine, index: usize, token: Option<&'t T>) -> bool {
            match self {
                TestPeeker::WordBoundary => token
                    .as_ref()
                    .map_or(true, |tok| tok.is_word() ^ engine.is_word),
                TestPeeker::Save(slot) => {
                    engine.saves[*slot] = Some(index);
                    true
                }
            }
        }
    }

    #[test]
    fn program() {
        use self::program::Instr::*;
        // /(ab?)(b?c)\b/
        let prog: Vec<program::Instr<char, TestEngine>> = vec![
            // 0: *? quantifier
            JSplit(3),
            // 1: match a token
            Any,
            // 2: repeat
            Jump(0),
            // 3: save start of match
            Peek(TestPeeker::Save(0)),
            // 4: save start of first subgroup
            Peek(TestPeeker::Save(2)),
            // 5: a
            Token('a'),
            // 6: optional b
            Split(8),
            // 7: b
            Token('b'),
            // 8: save end of first subgroup
            Peek(TestPeeker::Save(3)),
            // 9: save start of second subgroup
            Peek(TestPeeker::Save(4)),
            // 10: optional b
            Split(12),
            // 11: b
            Token('b'),
            // 12: c
            Token('c'),
            // 13: save end of second subgroup
            Peek(TestPeeker::Save(5)),
            // 14: word boundary
            WordBoundary,
            // 15: save end of match
            Peek(TestPeeker::Save(1)),
            // 16: end of match
            Match,
        ];
        let num_slots = 6;
        let program = program::Program::new(prog, num_slots);
        let saves = program.exec("ducabc ".chars());
        assert_eq!(
            saves.iter().map(|engine| &engine.saves).collect::<Vec<_>>(),
            &[
                &[Some(3), Some(6), Some(3), Some(5), Some(5), Some(6)],
                &[Some(3), Some(6), Some(3), Some(4), Some(4), Some(6)],
            ]
        );
    }

    // #[test]
    // fn ast() {
    //     use self::ast::Regex::*;
    //     // /(ab?)(b?c)a\b/
    //     let tree = Concat(vec![
    //         Capture(Box::new(Concat(vec![
    //             Literal(vec!['a']),
    //             Repeat(Box::new(Literal(vec!['b'])), ast::Repeater::ZeroOrOne(true)),
    //         ]))),
    //         Capture(Box::new(Concat(vec![
    //             Repeat(Box::new(Literal(vec!['b'])), ast::Repeater::ZeroOrOne(true)),
    //             Literal(vec!['c']),
    //         ]))),
    //         WordBoundary,
    //     ]);
    //     let prog = tree.compile();
    //     let saves = prog.exec("ducabc ".chars());
    //     assert_eq!(
    //         saves,
    //         vec![
    //             vec![Some(3), Some(6), Some(3), Some(5), Some(5), Some(6)],
    //             vec![Some(3), Some(6), Some(3), Some(4), Some(4), Some(6)],
    //         ]
    //     );
    // }
}
