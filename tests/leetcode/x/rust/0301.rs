use std::collections::BTreeSet;
use std::io::{self, Read};

fn dfs(chars: &[char], i: usize, left: i32, right: i32, balance: i32, path: &mut String, ans: &mut BTreeSet<String>) {
    if i == chars.len() {
        if left == 0 && right == 0 && balance == 0 {
            ans.insert(path.clone());
        }
        return;
    }
    let ch = chars[i];
    let len = path.len();
    if ch == '(' {
        if left > 0 {
            dfs(chars, i + 1, left - 1, right, balance, path, ans);
        }
        path.push(ch);
        dfs(chars, i + 1, left, right, balance + 1, path, ans);
        path.truncate(len);
    } else if ch == ')' {
        if right > 0 {
            dfs(chars, i + 1, left, right - 1, balance, path, ans);
        }
        if balance > 0 {
            path.push(ch);
            dfs(chars, i + 1, left, right, balance - 1, path, ans);
            path.truncate(len);
        }
    } else {
        path.push(ch);
        dfs(chars, i + 1, left, right, balance, path, ans);
        path.truncate(len);
    }
}

fn solve(s: &str) -> Vec<String> {
    let mut left_remove = 0;
    let mut right_remove = 0;
    for ch in s.chars() {
        if ch == '(' {
            left_remove += 1;
        } else if ch == ')' {
            if left_remove > 0 {
                left_remove -= 1;
            } else {
                right_remove += 1;
            }
        }
    }
    let chars: Vec<char> = s.chars().collect();
    let mut ans = BTreeSet::new();
    let mut path = String::new();
    dfs(&chars, 0, left_remove, right_remove, 0, &mut path, &mut ans);
    ans.into_iter().collect()
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let lines: Vec<&str> = input.lines().collect();
    if lines.is_empty() {
        return;
    }
    let t: usize = lines[0].trim().parse().unwrap();
    let mut out = Vec::new();
    for tc in 0..t {
        let ans = solve(lines[tc + 1]);
        let mut block = vec![ans.len().to_string()];
        block.extend(ans);
        out.push(block.join("\n"));
    }
    print!("{}", out.join("\n\n"));
}
