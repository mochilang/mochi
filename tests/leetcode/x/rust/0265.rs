use std::io::{self, Read};

fn solve(costs: Vec<Vec<i64>>) -> i64 {
    if costs.is_empty() {
        return 0;
    }
    let mut prev = costs[0].clone();
    for row in costs.iter().skip(1) {
        let mut min1 = i64::MAX;
        let mut min2 = i64::MAX;
        let mut idx1 = 0usize;
        for (i, &v) in prev.iter().enumerate() {
            if v < min1 {
                min2 = min1;
                min1 = v;
                idx1 = i;
            } else if v < min2 {
                min2 = v;
            }
        }
        let mut cur = vec![0; prev.len()];
        for i in 0..prev.len() {
            cur[i] = row[i] + if i == idx1 { min2 } else { min1 };
        }
        prev = cur;
    }
    *prev.iter().min().unwrap()
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let toks: Vec<i64> = input.split_whitespace().map(|s| s.parse().unwrap()).collect();
    if toks.is_empty() {
        return;
    }
    let mut idx = 0usize;
    let t = toks[idx] as usize;
    idx += 1;
    let mut out = Vec::with_capacity(t);
    for _ in 0..t {
        let n = toks[idx] as usize;
        idx += 1;
        let k = toks[idx] as usize;
        idx += 1;
        let mut costs = vec![vec![0; k]; n];
        for i in 0..n {
            for j in 0..k {
                costs[i][j] = toks[idx];
                idx += 1;
            }
        }
        out.push(solve(costs).to_string());
    }
    print!("{}", out.join("\n"));
}
