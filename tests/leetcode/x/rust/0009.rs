use std::io::{self, Read};

fn is_palindrome(mut x: i64) -> bool {
    if x < 0 {
        return false;
    }
    let original = x;
    let mut rev = 0i64;
    while x > 0 {
        rev = rev * 10 + x % 10;
        x /= 10;
    }
    rev == original
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let mut it = input.split_whitespace();
    let t: usize = match it.next() {
        Some(v) => v.parse().unwrap(),
        None => return,
    };
    let mut out = Vec::new();
    for _ in 0..t {
        let x: i64 = it.next().unwrap().parse().unwrap();
        out.push(if is_palindrome(x) { "true" } else { "false" });
    }
    print!("{}", out.join("\n"));
}
