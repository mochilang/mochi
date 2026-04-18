use std::io::{self, Read};

fn first_missing_positive(nums: &mut [i32]) -> i32 {
    let n = nums.len();
    let mut i = 0usize;
    while i < n {
        let v = nums[i];
        if v >= 1 && (v as usize) <= n && nums[v as usize - 1] != v {
            nums.swap(i, v as usize - 1);
        } else {
            i += 1;
        }
    }
    for (i, &v) in nums.iter().enumerate() {
        if v != i as i32 + 1 {
            return i as i32 + 1;
        }
    }
    n as i32 + 1
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let lines: Vec<String> = input.lines().map(|s| s.trim().to_string()).collect();
    if lines.is_empty() || lines[0].is_empty() {
        return;
    }
    let mut idx = 0usize;
    let t: usize = lines[idx].parse().unwrap();
    idx += 1;
    let mut out: Vec<String> = Vec::with_capacity(t);
    for _ in 0..t {
        let n: usize = lines[idx].parse().unwrap();
        idx += 1;
        let mut nums = vec![0i32; n];
        for i in 0..n {
            nums[i] = lines[idx].parse().unwrap();
            idx += 1;
        }
        out.push(first_missing_positive(&mut nums).to_string());
    }
    print!("{}", out.join("\n"));
}
