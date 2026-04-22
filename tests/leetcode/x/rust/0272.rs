use std::io::{self, Read};

fn solve(values: &[i64], target: f64, k: usize) -> Vec<i64> {
    let mut right = 0usize;
    while right < values.len() && (values[right] as f64) < target {
        right += 1;
    }
    let mut left = right as isize - 1;
    let mut ans = Vec::with_capacity(k);
    while ans.len() < k {
        if left < 0 {
            ans.push(values[right]);
            right += 1;
        } else if right >= values.len() {
            ans.push(values[left as usize]);
            left -= 1;
        } else if ((values[left as usize] as f64) - target).abs() <= ((values[right] as f64) - target).abs() {
            ans.push(values[left as usize]);
            left -= 1;
        } else {
            ans.push(values[right]);
            right += 1;
        }
    }
    ans
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let toks: Vec<String> = input.split_whitespace().map(|s| s.to_string()).collect();
    if toks.is_empty() {
        return;
    }
    let mut idx = 0usize;
    let t: usize = toks[idx].parse().unwrap();
    idx += 1;
    let mut blocks = Vec::new();
    for _ in 0..t {
        let n: usize = toks[idx].parse().unwrap();
        idx += 1;
        let mut values = vec![0i64; n];
        for i in 0..n {
            values[i] = toks[idx].parse().unwrap();
            idx += 1;
        }
        let target: f64 = toks[idx].parse().unwrap();
        idx += 1;
        let k: usize = toks[idx].parse().unwrap();
        idx += 1;
        let ans = solve(&values, target, k);
        let mut lines = Vec::new();
        lines.push(ans.len().to_string());
        lines.extend(ans.into_iter().map(|x| x.to_string()));
        blocks.push(lines.join("\n"));
    }
    print!("{}", blocks.join("\n\n"));
}
