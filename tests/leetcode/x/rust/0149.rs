use std::collections::HashMap;
use std::io::{self, Read};

fn gcd(mut a: i32, mut b: i32) -> i32 {
    while b != 0 {
        let t = a % b;
        a = b;
        b = t;
    }
    a.abs()
}

fn max_points(points: Vec<(i32, i32)>) -> i32 {
    let n = points.len();
    if n <= 2 {
        return n as i32;
    }
    let mut best = 0;
    for i in 0..n {
        let mut slopes: HashMap<String, i32> = HashMap::new();
        let mut local = 0;
        for j in i + 1..n {
            let mut dx = points[j].0 - points[i].0;
            let mut dy = points[j].1 - points[i].1;
            let g = gcd(dx, dy);
            dx /= g;
            dy /= g;
            if dx < 0 {
                dx = -dx;
                dy = -dy;
            } else if dx == 0 {
                dy = 1;
            } else if dy == 0 {
                dx = 1;
            }
            let key = format!("{dy}/{dx}");
            let count = slopes.entry(key).or_insert(0);
            *count += 1;
            local = local.max(*count);
        }
        best = best.max(local + 1);
    }
    best
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
        let mut points = Vec::new();
        for _ in 0..n {
            let parts: Vec<i32> = lines[idx]
                .split_whitespace()
                .map(|x| x.parse().unwrap_or(0))
                .collect();
            idx += 1;
            points.push((parts[0], parts[1]));
        }
        out.push(max_points(points).to_string());
    }
    print!("{}", out.join("\n\n"));
}
