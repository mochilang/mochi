use std::collections::HashMap;
use std::io::{self, Read};

fn bucket_id(x: i64, size: i64) -> i64 {
    if x >= 0 { x / size } else { -((-x - 1) / size) - 1 }
}

fn solve(nums: Vec<i64>, index_diff: usize, value_diff: i64) -> bool {
    let size = value_diff + 1;
    let mut buckets: HashMap<i64, i64> = HashMap::new();
    for (i, &x) in nums.iter().enumerate() {
        let bid = bucket_id(x, size);
        if buckets.contains_key(&bid) {
            return true;
        }
        if let Some(&y) = buckets.get(&(bid - 1)) {
            if (x - y).abs() <= value_diff {
                return true;
            }
        }
        if let Some(&y) = buckets.get(&(bid + 1)) {
            if (x - y).abs() <= value_diff {
                return true;
            }
        }
        buckets.insert(bid, x);
        if i >= index_diff {
            buckets.remove(&bucket_id(nums[i - index_diff], size));
        }
    }
    false
}

fn main() {
    let mut s = String::new();
    io::stdin().read_to_string(&mut s).unwrap();
    let mut it = s.split_whitespace();
    let t: usize = match it.next() { Some(v) => v.parse().unwrap(), None => return };
    let mut out = Vec::new();
    for _ in 0..t {
        let n: usize = it.next().unwrap().parse().unwrap();
        let mut nums = Vec::new();
        for _ in 0..n {
            nums.push(it.next().unwrap().parse::<i64>().unwrap());
        }
        let index_diff: usize = it.next().unwrap().parse().unwrap();
        let value_diff: i64 = it.next().unwrap().parse().unwrap();
        out.push(if solve(nums, index_diff, value_diff) { "true" } else { "false" }.to_string());
    }
    print!("{}", out.join("\n"));
}
