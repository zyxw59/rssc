pub mod category;
pub mod parser;
pub mod rule;
pub mod token;
pub mod unicode;
mod utils;

use std::io;

// #[test]
// fn rssc() {
//     assert!(rssc::parse_Pattern("22").is_ok());
//     assert!(rssc::parse_Pattern("22|33").is_ok());
// }

fn main() {
    let stdin = io::stdin();
    let mut segments = token::segment::SegmentMap::new();
    for line in token::Tokens::new(stdin.lock(), &mut segments).lines() {
        match line {
            Ok(line) => {
                println!("{line:?}");
            }
            Err(err) => {
                println!("Error: {err}");
                break;
            }
        }
    }
}
