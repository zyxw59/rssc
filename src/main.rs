#[macro_use] extern crate lazy_static;
extern crate radix_trie;
extern crate regex;
extern crate unicode_categories;
extern crate unicode_normalization;

pub mod re;
pub mod rssc;
pub mod ast;
pub mod category;
pub mod token;
pub mod segment;

use std::io::{self, BufRead};

// #[test]
// fn rssc() {
//     assert!(rssc::parse_Pattern("22").is_ok());
//     assert!(rssc::parse_Pattern("22|33").is_ok());
// }

fn main() {
    let stdin = io::stdin();
    let segments = token::segment::SegmentMap::clone_from_vec(&Vec::new());
    /*
    let tokens = token::Tokens::new(stdin.lock(), segments);
    for t in tokens {
        print!("{:?} ", t);
        if t == token::Token::from_u8(b'\n') {
            println!("");
        }
    }
    */
    for line in stdin.lock().lines() {
        match line {
            Ok(line) => {
                let line = line + "\n";
                let tokens = token::Tokens::new(line.as_ref(), segments.clone());
                println!("{:?}", tokens.collect::<Vec<_>>());
                /*
                let s: &str = &*line.trim();
                println!("{:?}", rssc::parse_Pattern(s).unwrap());
                println!("{}", rssc::parse_Pattern(s).unwrap());
                */
            },
            Err(err) => {println!("Error: {}", err); break;},
        }
    }
}
