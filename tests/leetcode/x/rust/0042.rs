use std::io::{self, Read};

fn trap(height: &[i32]) -> i32 {
    let mut left: usize = 0;
    let mut right: usize = height.len() - 1;
    let mut left_max = 0;
    let mut right_max = 0;
    let mut water = 0;
    while left <= right {
        if left_max <= right_max {
            if height[left] < left_max { water += left_max - height[left]; } else { left_max = height[left]; }
            left += 1;
        } else {
            if height[right] < right_max { water += right_max - height[right]; } else { right_max = height[right]; }
            if right == 0 { break; }
            right -= 1;
        }
    }
    water
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let lines: Vec<String> = input.lines().map(|s| s.trim().to_string()).collect();
    if lines.is_empty() || lines[0].is_empty() { return; }
    let mut idx = 0usize;
    let t: usize = lines[idx].parse().unwrap(); idx += 1;
    let mut out = Vec::with_capacity(t);
    for _ in 0..t {
      let n: usize = lines[idx].parse().unwrap(); idx += 1;
      let mut arr = vec![0i32; n];
      for i in 0..n { arr[i] = lines[idx].parse().unwrap(); idx += 1; }
      out.push(trap(&arr).to_string());
    }
    print!("{}", out.join("\n"));
}
