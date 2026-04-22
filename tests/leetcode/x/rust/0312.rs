use std::io::{self, Read};

fn max_coins(nums: &[i32]) -> i32 {
    let mut vals = Vec::with_capacity(nums.len() + 2);
    vals.push(1);
    vals.extend_from_slice(nums);
    vals.push(1);
    let n = vals.len();
    let mut dp = vec![vec![0i32; n]; n];
    for length in 2..n {
        for left in 0..(n - length) {
            let right = left + length;
            for k in (left + 1)..right {
                dp[left][right] = dp[left][right].max(dp[left][k] + dp[k][right] + vals[left] * vals[k] * vals[right]);
            }
        }
    }
    dp[0][n - 1]
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let data: Vec<i32> = input.split_whitespace().filter_map(|s| s.parse().ok()).collect();
    if data.is_empty() {
        return;
    }
    let mut idx = 0usize;
    let t = data[idx] as usize;
    idx += 1;
    let mut out = Vec::new();
    for _ in 0..t {
        let n = data[idx] as usize;
        idx += 1;
        let nums = data[idx..idx + n].to_vec();
        idx += n;
        out.push(max_coins(&nums).to_string());
    }
    print!("{}", out.join("\n\n"));
}
