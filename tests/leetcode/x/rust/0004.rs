use std::io::{self, Read};

fn median(a: &[i32], b: &[i32]) -> f64 {
    let mut m = Vec::with_capacity(a.len() + b.len());
    let (mut i, mut j) = (0usize, 0usize);
    while i < a.len() && j < b.len() {
        if a[i] <= b[j] { m.push(a[i]); i += 1; } else { m.push(b[j]); j += 1; }
    }
    m.extend_from_slice(&a[i..]);
    m.extend_from_slice(&b[j..]);
    if m.len() % 2 == 1 { m[m.len() / 2] as f64 } else { (m[m.len() / 2 - 1] + m[m.len() / 2]) as f64 / 2.0 }
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
        let n: usize = lines.get(idx).unwrap_or(&"0").trim().parse().unwrap_or(0); idx += 1;
        let mut a = Vec::new();
        for _ in 0..n { a.push(lines.get(idx).unwrap_or(&"0").trim().parse().unwrap_or(0)); idx += 1; }
        let m: usize = lines.get(idx).unwrap_or(&"0").trim().parse().unwrap_or(0); idx += 1;
        let mut b = Vec::new();
        for _ in 0..m { b.push(lines.get(idx).unwrap_or(&"0").trim().parse().unwrap_or(0)); idx += 1; }
        out.push(format!("{:.1}", median(&a, &b)));
    }
    print!("{}", out.join("\n"));
}
