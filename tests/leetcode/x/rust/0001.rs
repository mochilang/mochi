use std::io::{self, Read};

fn two_sum(nums: &[i32], target: i32) -> (usize, usize) {
    for i in 0..nums.len() {
        for j in i + 1..nums.len() {
            if nums[i] + nums[j] == target {
                return (i, j);
            }
        }
    }
    (0, 0)
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    if input.trim().is_empty() {
        return;
    }
    let values: Vec<i32> = input.split_whitespace().map(|s| s.parse().unwrap()).collect();
    let mut idx = 0usize;
    let t = values[idx] as usize;
    idx += 1;
    let mut out = Vec::new();
    for _ in 0..t {
        let n = values[idx] as usize;
        let target = values[idx + 1];
        idx += 2;
        let nums = &values[idx..idx + n];
        idx += n;
        let (a, b) = two_sum(nums, target);
        out.push(format!("{} {}", a, b));
    }
    print!("{}", out.join("\n"));
}
