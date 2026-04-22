use std::io::{self, Read};

fn find_min(nums: Vec<i32>) -> i32 {
    let mut left = 0usize;
    let mut right = nums.len() - 1;
    while left < right {
        let mid = (left + right) / 2;
        if nums[mid] < nums[right] {
            right = mid;
        } else if nums[mid] > nums[right] {
            left = mid + 1;
        } else {
            right -= 1;
        }
    }
    nums[left]
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let lines: Vec<&str> = input.lines().collect();
    if lines.is_empty() {
        return;
    }
    let tc: usize = lines[0].trim().parse().unwrap_or(0);
    let mut idx = 1usize;
    let mut out = Vec::new();
    for _ in 0..tc {
        let n: usize = lines[idx].trim().parse().unwrap_or(0);
        idx += 1;
        let mut nums = Vec::new();
        for _ in 0..n {
            nums.push(lines[idx].trim().parse().unwrap_or(0));
            idx += 1;
        }
        out.push(find_min(nums).to_string());
    }
    print!("{}", out.join("\n\n"));
}
