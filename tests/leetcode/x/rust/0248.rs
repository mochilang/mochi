use std::io::{self, Read};

const PAIRS: [(char, char); 5] = [('0', '0'), ('1', '1'), ('6', '9'), ('8', '8'), ('9', '6')];

fn build(n: usize, m: usize) -> Vec<String> {
    if n == 0 {
        return vec![String::new()];
    }
    if n == 1 {
        return vec!["0".into(), "1".into(), "8".into()];
    }
    let mids = build(n - 2, m);
    let mut res = Vec::new();
    for mid in mids {
        for (a, b) in PAIRS {
            if n == m && a == '0' {
                continue;
            }
            res.push(format!("{a}{mid}{b}"));
        }
    }
    res
}

fn count_range(low: &str, high: &str) -> i32 {
    let mut ans = 0;
    for len in low.len()..=high.len() {
        for s in build(len, len) {
            if len == low.len() && s.as_str() < low {
                continue;
            }
            if len == high.len() && s.as_str() > high {
                continue;
            }
            ans += 1;
        }
    }
    ans
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let lines: Vec<&str> = input.lines().collect();
    if lines.is_empty() {
        return;
    }
    let t: usize = lines[0].trim().parse().unwrap();
    let mut out = Vec::with_capacity(t);
    let mut idx = 1;
    for _ in 0..t {
        out.push(count_range(lines[idx].trim(), lines[idx + 1].trim()).to_string());
        idx += 2;
    }
    print!("{}", out.join("\n"));
}
