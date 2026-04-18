use std::io::{self, Read};

fn solve(a: &[i32]) -> i32 {
    let mut best = 0;
    for i in 0..a.len() {
        let mut mn = a[i];
        for j in i..a.len() {
            if a[j] < mn { mn = a[j]; }
            let area = mn * (j - i + 1) as i32;
            if area > best { best = area; }
        }
    }
    best
}

fn main() {
    let mut s = String::new();
    io::stdin().read_to_string(&mut s).unwrap();
    let mut it = s.split_whitespace();
    let t: usize = match it.next() { Some(v) => v.parse().unwrap(), None => return };
    let mut out = Vec::new();
    for _ in 0..t {
        let n: usize = it.next().unwrap().parse().unwrap();
        let mut a = Vec::with_capacity(n);
        for _ in 0..n {
            a.push(it.next().unwrap().parse::<i32>().unwrap());
        }
        out.push(solve(&a).to_string());
    }
    print!("{}", out.join("\n"));
}
