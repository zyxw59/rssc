extern crate radix_trie;
extern crate unicode_categories;
extern crate unicode_normalization;

pub mod ast;
pub mod category;
pub mod parser;
pub mod re;
pub mod segment;
pub mod token;

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
                let tokens = parser::tokenizer::Tokens::new(line.as_ref(), segments.clone());
                println!("{:?}", tokens.collect::<Vec<_>>());
            }
            Err(err) => {
                println!("Error: {}", err);
                break;
            }
        }
    }
}
