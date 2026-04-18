use std::io::{self, Read};

fn expand(s: &[u8], mut left: i32, mut right: i32) -> (usize, usize) {
    while left >= 0 && (right as usize) < s.len() && s[left as usize] == s[right as usize] {
        left -= 1;
        right += 1;
    }
    ((left + 1) as usize, (right - left - 1) as usize)
}

fn longest_palindrome(s: &str) -> String {
    let bytes = s.as_bytes();
    let mut best_start = 0usize;
    let mut best_len = if bytes.is_empty() { 0 } else { 1 };
    for i in 0..bytes.len() {
        let (start1, len1) = expand(bytes, i as i32, i as i32);
        if len1 > best_len {
            best_start = start1;
            best_len = len1;
        }
        let (start2, len2) = expand(bytes, i as i32, i as i32 + 1);
        if len2 > best_len {
            best_start = start2;
            best_len = len2;
        }
    }
    s[best_start..best_start + best_len].to_string()
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    if input.is_empty() {
        return;
    }
    let mut lines = input.split('\n');
    let t: usize = lines.next().unwrap_or("").trim().parse().unwrap_or(0);
    let mut out = Vec::new();
    for _ in 0..t {
        let s = lines.next().unwrap_or("").trim_end_matches('\r');
        out.push(longest_palindrome(s));
    }
    print!("{}", out.join("\n"));
}
