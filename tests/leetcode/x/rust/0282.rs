use std::io::{self, Read};

fn solve(num: &str, target: i64) -> Vec<String> {
    let mut ans = Vec::new();
    fn dfs(num: &str, target: i64, i: usize, expr: String, value: i64, last: i64, ans: &mut Vec<String>) {
        if i == num.len() {
            if value == target {
                ans.push(expr);
            }
            return;
        }
        for j in i..num.len() {
            if j > i && num.as_bytes()[i] == b'0' {
                break;
            }
            let s = &num[i..=j];
            let n: i64 = s.parse().unwrap();
            if i == 0 {
                dfs(num, target, j + 1, s.to_string(), n, n, ans);
            } else {
                dfs(num, target, j + 1, format!("{expr}+{s}"), value + n, n, ans);
                dfs(num, target, j + 1, format!("{expr}-{s}"), value - n, -n, ans);
                dfs(num, target, j + 1, format!("{expr}*{s}"), value - last + last * n, last * n, ans);
            }
        }
    }
    dfs(num, target, 0, String::new(), 0, 0, &mut ans);
    ans.sort();
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
    let mut idx = 1;
    let mut blocks = Vec::new();
    for _ in 0..t {
        let num = lines[idx].trim();
        let target: i64 = lines[idx + 1].trim().parse().unwrap();
        idx += 2;
        let ans = solve(num, target);
        let mut part = Vec::new();
        part.push(ans.len().to_string());
        part.extend(ans);
        blocks.push(part.join("\n"));
    }
    print!("{}", blocks.join("\n\n"));
}
