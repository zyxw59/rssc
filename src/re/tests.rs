use super::{engine::Engine, program};

#[derive(Clone, Debug)]
pub struct TestEngine {
    saves: Vec<Option<usize>>,
    is_word: bool,
}

impl Engine<char> for TestEngine {
    /// Number of save slots.
    type Init = usize;
    type Consume = TestConsume;
    type Peek = TestPeek;

    fn initialize(num_slots: &Self::Init) -> Self {
        TestEngine {
            saves: vec![None; *num_slots],
            is_word: false,
        }
    }

    fn consume(&mut self, args: &Self::Consume, _index: usize, token: &char) -> bool {
        self.is_word = !token.is_whitespace();
        match args {
            TestConsume::Any => true,
            TestConsume::Token(expected) => expected == token,
        }
    }

    fn peek(&mut self, args: &Self::Peek, index: usize, token: Option<&char>) -> bool {
        match args {
            TestPeek::WordBoundary => token
                .as_ref()
                .map_or(true, |tok| !tok.is_whitespace() ^ self.is_word),
            TestPeek::Save(slot) => {
                self.saves[*slot] = Some(index);
                true
            }
        }
    }
}

pub enum TestConsume {
    Any,
    Token(char),
}

pub enum TestPeek {
    WordBoundary,
    Save(usize),
}

#[test]
fn program() {
    use self::program::Instr::*;
    // /(ab?)(b?c)\b/
    let prog: Vec<program::Instr<char, TestEngine>> = vec![
        // 0: *? quantifier
        JSplit(3),
        // 1: match a token
        Consume(TestConsume::Any),
        // 2: repeat
        Jump(0),
        // 3: save start of match
        Peek(TestPeek::Save(0)),
        // 4: save start of first subgroup
        Peek(TestPeek::Save(2)),
        // 5: a
        Consume(TestConsume::Token('a')),
        // 6: optional b
        Split(8),
        // 7: b
        Consume(TestConsume::Token('b')),
        // 8: save end of first subgroup
        Peek(TestPeek::Save(3)),
        // 9: save start of second subgroup
        Peek(TestPeek::Save(4)),
        // 10: optional b
        Split(12),
        // 11: b
        Consume(TestConsume::Token('b')),
        // 12: c
        Consume(TestConsume::Token('c')),
        // 13: save end of second subgroup
        Peek(TestPeek::Save(5)),
        // 14: word boundary
        Peek(TestPeek::WordBoundary),
        // 15: save end of match
        Peek(TestPeek::Save(1)),
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
    let saves = program.exec("ducabc".chars());
    assert_eq!(
        saves.iter().map(|engine| &engine.saves).collect::<Vec<_>>(),
        &[
            &[Some(3), Some(6), Some(3), Some(5), Some(5), Some(6)],
            &[Some(3), Some(6), Some(3), Some(4), Some(4), Some(6)],
        ]
    );
    let saves = program.exec("ducabcd".chars());
    assert!(saves.is_empty());
}
