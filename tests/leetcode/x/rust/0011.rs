use std::io::{self, Read};

fn max_area(h: &[i32]) -> i32 {
    let mut left = 0usize;
    let mut right = h.len() - 1;
    let mut best = 0i32;
    while left < right {
        let height = h[left].min(h[right]);
        best = best.max((right - left) as i32 * height);
        if h[left] < h[right] { left += 1; } else { right -= 1; }
    }
    best
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    if input.is_empty() { return; }
    let lines: Vec<&str> = input.split('\n').collect();
    let t: usize = lines[0].trim().parse().unwrap_or(0);
    let mut idx = 1usize;
    let mut out = Vec::new();
    for _ in 0..t {
        let n: usize = lines.get(idx).unwrap_or(&"0").trim().parse().unwrap_or(0);
        idx += 1;
        let mut h = Vec::new();
        for _ in 0..n {
            h.push(lines.get(idx).unwrap_or(&"0").trim().parse().unwrap_or(0));
            idx += 1;
        }
        out.push(max_area(&h).to_string());
    }
    print!("{}", out.join("\n"));
}
