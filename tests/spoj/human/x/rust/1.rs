// SPOJ: Life, the Universe, and Everything
// https://www.spoj.com/problems/TEST
use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    for line in input.lines() {
        if let Ok(n) = line.trim().parse::<i32>() {
            if n == 42 {
                break;
            }
            println!("{}", n);
        }
    }
}
