use std::io::{self, Read};

fn pick(nums: &[i32], k: usize) -> Vec<i32> {
    let mut drop = nums.len() - k;
    let mut stack = Vec::new();
    for &x in nums {
        while drop > 0 && !stack.is_empty() && *stack.last().unwrap() < x {
            stack.pop();
            drop -= 1;
        }
        stack.push(x);
    }
    stack.truncate(k);
    stack
}

fn greater(a: &[i32], mut i: usize, b: &[i32], mut j: usize) -> bool {
    while i < a.len() && j < b.len() && a[i] == b[j] {
        i += 1;
        j += 1;
    }
    j == b.len() || (i < a.len() && a[i] > b[j])
}

fn merge(a: &[i32], b: &[i32]) -> Vec<i32> {
    let mut out = Vec::with_capacity(a.len() + b.len());
    let (mut i, mut j) = (0usize, 0usize);
    while i < a.len() || j < b.len() {
        if greater(a, i, b, j) {
            out.push(a[i]);
            i += 1;
        } else {
            out.push(b[j]);
            j += 1;
        }
    }
    out
}

fn max_number(nums1: &[i32], nums2: &[i32], k: usize) -> Vec<i32> {
    let mut best = Vec::new();
    let start = k.saturating_sub(nums2.len());
    let end = k.min(nums1.len());
    for take in start..=end {
        let cand = merge(&pick(nums1, take), &pick(nums2, k - take));
        if greater(&cand, 0, &best, 0) {
            best = cand;
        }
    }
    best
}

fn fmt_list(a: &[i32]) -> String {
    format!("[{}]", a.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(","))
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
        let n1 = data[idx] as usize;
        idx += 1;
        let nums1 = data[idx..idx + n1].to_vec();
        idx += n1;
        let n2 = data[idx] as usize;
        idx += 1;
        let nums2 = data[idx..idx + n2].to_vec();
        idx += n2;
        let k = data[idx] as usize;
        idx += 1;
        out.push(fmt_list(&max_number(&nums1, &nums2, k)));
    }
    print!("{}", out.join("\n\n"));
}
