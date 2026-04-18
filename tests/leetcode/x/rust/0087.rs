use std::collections::HashMap;
use std::io::{self, Read};

fn is_scramble(s1: &str, s2: &str) -> bool {
    fn dfs(i1: usize, i2: usize, len: usize, s1: &[u8], s2: &[u8], memo: &mut HashMap<(usize, usize, usize), bool>) -> bool {
        if let Some(v) = memo.get(&(i1, i2, len)) { return *v; }
        if s1[i1..i1 + len] == s2[i2..i2 + len] {
            memo.insert((i1, i2, len), true);
            return true;
        }
        let mut cnt = [0i32; 26];
        for i in 0..len {
            cnt[(s1[i1 + i] - b'a') as usize] += 1;
            cnt[(s2[i2 + i] - b'a') as usize] -= 1;
        }
        if cnt.iter().any(|&v| v != 0) {
            memo.insert((i1, i2, len), false);
            return false;
        }
        for k in 1..len {
            if dfs(i1, i2, k, s1, s2, memo) && dfs(i1 + k, i2 + k, len - k, s1, s2, memo)
                || dfs(i1, i2 + len - k, k, s1, s2, memo) && dfs(i1 + k, i2, len - k, s1, s2, memo) {
                memo.insert((i1, i2, len), true);
                return true;
            }
        }
        memo.insert((i1, i2, len), false);
        false
    }
    dfs(0, 0, s1.len(), s1.as_bytes(), s2.as_bytes(), &mut HashMap::new())
}

fn main() {
    let mut s = String::new();
    io::stdin().read_to_string(&mut s).unwrap();
    let lines: Vec<&str> = s.lines().collect();
    if lines.is_empty() { return; }
    let t: usize = lines[0].trim().parse().unwrap();
    let mut out = Vec::new();
    for i in 0..t {
        out.push(if is_scramble(lines[1 + 2 * i], lines[2 + 2 * i]) { "true" } else { "false" });
    }
    print!("{}", out.join("\n"));
}
