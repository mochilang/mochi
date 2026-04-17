use std::io::{self, Read};

fn match_at(s: &[u8], p: &[u8], i: usize, j: usize) -> bool {
    if j >= p.len() {
        return i >= s.len();
    }
    let first = i < s.len() && (p[j] == b'.' || s[i] == p[j]);
    if j + 1 < p.len() && p[j + 1] == b'*' {
        return match_at(s, p, i, j + 2) || (first && match_at(s, p, i + 1, j));
    }
    first && match_at(s, p, i + 1, j + 1)
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    if input.is_empty() { return; }
    let lines: Vec<&str> = input.split('\n').map(|s| s.trim_end_matches('\r')).collect();
    let t: usize = lines[0].trim().parse().unwrap_or(0);
    let mut idx = 1usize;
    let mut out = Vec::new();
    for _ in 0..t {
        let s = lines.get(idx).copied().unwrap_or(""); idx += 1;
        let p = lines.get(idx).copied().unwrap_or(""); idx += 1;
        out.push(if match_at(s.as_bytes(), p.as_bytes(), 0, 0) { "true" } else { "false" });
    }
    print!("{}", out.join("\n"));
}
