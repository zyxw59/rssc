use super::segment::SegmentMap;
use super::re::{Instr, Program};

#[cfg(test)]
mod tests {
    use super::*;
    use unicode_normalization::UnicodeNormalization;

    #[test]
    fn multiple_possible_segmentations() {
        let line = String::from("antś\n");
        let line = line.chars().nfd();
        let segments = SegmentMap::clone_from_vec(
            &vec![
                vec!['a', 'n'],
                vec!['n', 't'],
                vec!['t', 's'],
            ]);
        let prog = matcher(&segments);
        println!("{}", prog);

        let saves = prog.exec(line);
        assert_eq!(saves, Some(vec![0, 2, 3, 5]));
    }

    #[test]
    fn combining_double() {
        let line = String::from("t͜s\n");
        let line = line.chars().nfd();
        let segments = SegmentMap::clone_from_vec(&Vec::new());
        let prog = matcher(&segments);
        println!("{}", prog);

        let saves = prog.exec(line);
        assert_eq!(saves, Some(vec![0, 3]));
    }

    #[test]
    fn backslash() {
        let line = String::from("\\.\n");
        let line = line.chars().nfd();
        let segments = SegmentMap::clone_from_vec(&Vec::new());
        let prog = matcher(&segments);
        println!("{}", prog);

        let saves = prog.exec(line);
        assert_eq!(saves, Some(vec![0, 2]));
    }
}

/// Construct a regex program to split a line into segments.
pub fn matcher(segments: &SegmentMap) -> Program {
    // the instructions
    let mut prog = Vec::new();

    // save instruction, to be performed at the end of each token
    prog.push(Instr::Save);
    // match a control character
    let mut split = prog.len();
    prog.push(Instr::Split(0));
    prog.push(Instr::ControlChar);
    prog.push(Instr::Jump(0));
    // match a backslash-escaped character, which is considered as a single base character
    prog[split] = Instr::Split(prog.len());
    split = prog.len();
    prog.push(Instr::Split(0));
    prog.push(Instr::Char('\\'));
    prog.push(Instr::Any);
    let escape_jump = prog.len();
    prog.push(Instr::Jump(0));
    // match a newline, and therefore the end of the string
    prog[split] = Instr::Split(prog.len());
    split = prog.len();
    prog.push(Instr::Split(0));
    prog.push(Instr::Char('\n'));
    prog.push(Instr::Match);
    // match user-defined segments
    for seg in segments.iter() {
        prog[split] = Instr::Split(prog.len());
        split = prog.len();
        prog.push(Instr::Split(0));
        for c in seg {
            prog.push(Instr::Char(*c));
        }
        prog.push(Instr::Jump(0));
    }
    // match a base character followed by any number of combining characters
    let base = prog.len();
    prog[escape_jump] = Instr::Jump(base + 1);
    prog[split] = Instr::Split(base);
    prog.push(Instr::BaseChar);
    prog.push(Instr::Split(base + 4));
    prog.push(Instr::CombiningChar);
    prog.push(Instr::Jump(base + 1));
    prog.push(Instr::Split(0));
    prog.push(Instr::CombiningDouble);
    prog.push(Instr::Jump(base));

    Program::new(prog, 0)
}
