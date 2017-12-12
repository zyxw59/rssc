#[cfg(test)]
mod tests {
    use super::ast;
    use super::program;

    #[test]
    fn program() {
        use self::program::Instr::*;
        // /(ab?)(b?c)\b/
        let prog = vec![
            // 0: *? quantifier
            JSplit(3),
            // 1: match a token
            Any,
            // 2: repeat
            Jump(0),
            // 3: save start of match
            Save(0),
            // 4: save start of first subgroup
            Save(2),
            // 5: a
            Token('a'),
            // 6: optional b
            Split(8),
            // 7: b
            Token('b'),
            // 8: save end of first subgroup
            Save(3),
            // 9: save start of second subgroup
            Save(4),
            // 10: optional b
            Split(12),
            // 11: b
            Token('b'),
            // 12: c
            Token('c'),
            // 13: save end of second subgroup
            Save(5),
            // 14: word boundary
            WordBoundary,
            // 15: save end of match
            Save(1),
            // 16: end of match
            Match,
        ];
        let num_slots = 6;
        let program = program::Program::new(prog, num_slots);
        let saves = program.exec("ducabc ".chars());
        assert_eq!(saves,
                   vec![
                   vec![Some(3), Some(6), Some(3), Some(5), Some(5), Some(6)],
                   vec![Some(3), Some(6), Some(3), Some(4), Some(4), Some(6)],
                   ]);
    }

    #[test]
    fn ast() {
        use self::ast::Regex::*;
        // /(ab?)(b?c)a\b/
        let tree = Concat(vec![
            Capture(Box::new(Concat(vec![
                Literal(vec!['a']),
                Repeat(
                    Box::new(Literal(vec!['b'])),
                    ast::Repeater::ZeroOrOne(true)),
            ]))),
            Capture(Box::new(Concat(vec![
                Repeat(
                    Box::new(Literal(vec!['b'])),
                    ast::Repeater::ZeroOrOne(true)),
                Literal(vec!['c']),
            ]))),
            WordBoundary,
        ]);
        let prog = tree.compile();
        let saves = prog.exec("ducabc ".chars());
        assert_eq!(saves,
                   vec![
                   vec![Some(3), Some(6), Some(3), Some(5), Some(5), Some(6)],
                   vec![Some(3), Some(6), Some(3), Some(4), Some(4), Some(6)],
                   ]);
    }
}

pub mod ast;
pub mod program;
pub mod token;
