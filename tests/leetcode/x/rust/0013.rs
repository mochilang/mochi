use std::io::{self, Read};

fn value(c: char) -> i32 {
    match c {
        'I' => 1,
        'V' => 5,
        'X' => 10,
        'L' => 50,
        'C' => 100,
        'D' => 500,
        'M' => 1000,
        _ => 0,
    }
}

fn roman_to_int(s: &str) -> i32 {
    let chars: Vec<char> = s.chars().collect();
    let mut total = 0;
    for i in 0..chars.len() {
        let cur = value(chars[i]);
        let nxt = if i + 1 < chars.len() { value(chars[i + 1]) } else { 0 };
        total += if cur < nxt { -cur } else { cur };
    }
    total
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let tokens: Vec<&str> = input.split_whitespace().collect();
    if tokens.is_empty() {
        return;
    }
    let t: usize = tokens[0].parse().unwrap();
    let out: Vec<String> = tokens[1..1 + t].iter().map(|s| roman_to_int(s).to_string()).collect();
    print!("{}", out.join("\n"));
}
