#[macro_use] extern crate lazy_static;
extern crate regex;
pub mod rssc;
pub mod ast;

use std::io::{self, BufRead};

// #[test]
// fn rssc() {
//     assert!(rssc::parse_Pattern("22").is_ok());
//     assert!(rssc::parse_Pattern("22|33").is_ok());
// }

fn main() {
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        match line {
            Ok(line) => {
                let s: &str = &*line.trim();
                println!("{:?}", rssc::parse_Pattern(s).unwrap());
                println!("{}", rssc::parse_Pattern(s).unwrap());
            },
            Err(err) => {println!("Error: {}", err); break;},
        }
    }
}
