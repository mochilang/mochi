use std::io::{self, Read};

fn add_lists(a: &[i32], b: &[i32]) -> Vec<i32> {
    let mut out = Vec::with_capacity(a.len() + b.len() + 1);
    let (mut i, mut j, mut carry) = (0usize, 0usize, 0i32);
    while i < a.len() || j < b.len() || carry > 0 {
        let mut sum = carry;
        if i < a.len() { sum += a[i]; i += 1; }
        if j < b.len() { sum += b[j]; j += 1; }
        out.push(sum % 10);
        carry = sum / 10;
    }
    out
}

fn fmt(a: &[i32]) -> String {
    format!("[{}]", a.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(","))
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let tokens: Vec<&str> = input.split_whitespace().collect();
    if tokens.is_empty() { return; }
    let mut idx = 0usize;
    let t: usize = tokens[idx].parse().unwrap(); idx += 1;
    let mut lines = Vec::new();
    for _ in 0..t {
        let n: usize = tokens[idx].parse().unwrap(); idx += 1;
        let mut a = Vec::new();
        for _ in 0..n { a.push(tokens[idx].parse().unwrap()); idx += 1; }
        let m: usize = tokens[idx].parse().unwrap(); idx += 1;
        let mut b = Vec::new();
        for _ in 0..m { b.push(tokens[idx].parse().unwrap()); idx += 1; }
        lines.push(fmt(&add_lists(&a, &b)));
    }
    print!("{}", lines.join("\n"));
}
