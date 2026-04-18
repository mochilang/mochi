use std::io::{self, Read};

fn hist(h: &[i32]) -> i32 {
    let mut best = 0;
    for i in 0..h.len() {
        let mut mn = h[i];
        for j in i..h.len() {
            if h[j] < mn { mn = h[j]; }
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
        let rows: usize = it.next().unwrap().parse().unwrap();
        let cols: usize = it.next().unwrap().parse().unwrap();
        let mut h = vec![0; cols];
        let mut best = 0;
        for _ in 0..rows {
            let row = it.next().unwrap().as_bytes().to_vec();
            for c in 0..cols { h[c] = if row[c] == b'1' { h[c] + 1 } else { 0 }; }
            let area = hist(&h);
            if area > best { best = area; }
        }
        out.push(best.to_string());
    }
    print!("{}", out.join("\n"));
}
