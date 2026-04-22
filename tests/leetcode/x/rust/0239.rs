use std::collections::VecDeque;
use std::io::{self, Read};

fn solve(nums: &[i64], k: usize) -> Vec<i64> {
    let mut dq: VecDeque<usize> = VecDeque::new();
    let mut ans = Vec::new();
    for (i, &x) in nums.iter().enumerate() {
        while matches!(dq.front(), Some(&j) if j + k <= i) {
            dq.pop_front();
        }
        while matches!(dq.back(), Some(&j) if nums[j] <= x) {
            dq.pop_back();
        }
        dq.push_back(i);
        if i + 1 >= k {
            ans.push(nums[*dq.front().unwrap()]);
        }
    }
    ans
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let toks: Vec<i64> = input.split_whitespace().map(|s| s.parse().unwrap()).collect();
    if toks.is_empty() {
        return;
    }
    let mut idx = 0usize;
    let t = toks[idx] as usize;
    idx += 1;
    let mut blocks = Vec::new();
    for _ in 0..t {
        let n = toks[idx] as usize;
        idx += 1;
        let nums = &toks[idx..idx + n];
        idx += n;
        let k = toks[idx] as usize;
        idx += 1;
        let ans = solve(nums, k);
        let mut lines = Vec::with_capacity(ans.len() + 1);
        lines.push(ans.len().to_string());
        lines.extend(ans.into_iter().map(|x| x.to_string()));
        blocks.push(lines.join("\n"));
    }
    print!("{}", blocks.join("\n\n"));
}
