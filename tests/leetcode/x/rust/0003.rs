use std::collections::HashMap;
use std::io::{self, Read};

fn longest(s: &str) -> i32 {
    let mut last: HashMap<char, usize> = HashMap::new();
    let mut left = 0usize;
    let mut best = 0usize;
    for (right, ch) in s.chars().enumerate() {
        if let Some(&pos) = last.get(&ch) {
            if pos >= left {
                left = pos + 1;
            }
        }
        last.insert(ch, right);
        best = best.max(right - left + 1);
    }
    best as i32
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let mut lines = input.split('\n').map(|s| s.trim_end_matches('\r'));
    let first = match lines.next() {
        Some(v) if !v.trim().is_empty() => v.trim(),
        _ => return,
    };
    let t: usize = first.parse().unwrap();
    let out: Vec<String> = lines.take(t).map(|s| longest(s).to_string()).collect();
    print!("{}", out.join("\n"));
}
