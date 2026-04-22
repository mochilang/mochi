use std::io::{self, Read};

fn solve(s: String) -> String {
    let rev: String = s.chars().rev().collect();
    let combined = format!("{}#{}", s, rev);
    let bytes = combined.as_bytes();
    let mut pi = vec![0usize; bytes.len()];
    for i in 1..bytes.len() {
        let mut j = pi[i - 1];
        while j > 0 && bytes[i] != bytes[j] {
            j = pi[j - 1];
        }
        if bytes[i] == bytes[j] {
            j += 1;
        }
        pi[i] = j;
    }
    let keep = *pi.last().unwrap_or(&0);
    let original_len = s.len();
    let reversed: String = s.chars().rev().collect();
    reversed[..original_len - keep].to_string() + &s
}

fn main() {
    let mut s = String::new();
    io::stdin().read_to_string(&mut s).unwrap();
    let mut lines: Vec<&str> = s.split('\n').collect();
    if let Some(last) = lines.last() {
        if last.is_empty() {
            lines.pop();
        }
    }
    if lines.is_empty() {
        return;
    }
    let t: usize = lines[0].trim().parse().unwrap();
    let mut out = Vec::new();
    for i in 0..t {
        let line = if i + 1 < lines.len() { lines[i + 1].trim_end_matches('\r') } else { "" };
        out.push(solve(line.to_string()));
    }
    print!("{}", out.join("\n"));
}
