use std::io::{self, Read};

fn solve(s: &str, t: &str) -> i32 {
    let tb = t.as_bytes();
    let mut dp = vec![0i32; tb.len() + 1];
    dp[0] = 1;
    for &ch in s.as_bytes() {
        for j in (1..=tb.len()).rev() {
            if ch == tb[j - 1] {
                dp[j] += dp[j - 1];
            }
        }
    }
    dp[t.len()]
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let lines: Vec<&str> = input.lines().collect();
    if lines.is_empty() { return; }
    let tc: usize = lines[0].trim().parse().unwrap_or(0);
    let mut out = Vec::new();
    for i in 0..tc {
        out.push(solve(lines[1 + 2 * i], lines[2 + 2 * i]).to_string());
    }
    print!("{}", out.join("\n"));
}
