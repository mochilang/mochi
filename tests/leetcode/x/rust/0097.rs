use std::io::{self, Read};

fn solve(s1: &str, s2: &str, s3: &str) -> bool {
    let m = s1.len();
    let n = s2.len();
    if m + n != s3.len() { return false; }
    let b1 = s1.as_bytes();
    let b2 = s2.as_bytes();
    let b3 = s3.as_bytes();
    let mut dp = vec![vec![false; n + 1]; m + 1];
    dp[0][0] = true;
    for i in 0..=m {
        for j in 0..=n {
            if i > 0 && dp[i - 1][j] && b1[i - 1] == b3[i + j - 1] { dp[i][j] = true; }
            if j > 0 && dp[i][j - 1] && b2[j - 1] == b3[i + j - 1] { dp[i][j] = true; }
        }
    }
    dp[m][n]
}

fn main() {
    let mut s = String::new();
    io::stdin().read_to_string(&mut s).unwrap();
    let lines: Vec<&str> = s.lines().collect();
    if lines.is_empty() { return; }
    let t: usize = lines[0].trim().parse().unwrap();
    let mut out = Vec::new();
    for i in 0..t { out.push(if solve(lines[1 + 3*i], lines[2 + 3*i], lines[3 + 3*i]) { "true" } else { "false" }); }
    print!("{}", out.join("\n"));
}
