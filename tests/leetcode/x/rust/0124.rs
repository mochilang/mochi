use std::io::{self, Read};

fn solve(vals: &[i32], ok: &[bool]) -> i32 {
    fn dfs(i: usize, vals: &[i32], ok: &[bool], best: &mut i32) -> i32 {
        if i >= vals.len() || !ok[i] { return 0; }
        let left = 0.max(dfs(2 * i + 1, vals, ok, best));
        let right = 0.max(dfs(2 * i + 2, vals, ok, best));
        *best = (*best).max(vals[i] + left + right);
        vals[i] + left.max(right)
    }
    let mut best = -1_000_000_000;
    dfs(0, vals, ok, &mut best);
    best
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let lines: Vec<&str> = input.lines().collect();
    if lines.is_empty() { return; }
    let tc: usize = lines[0].trim().parse().unwrap_or(0);
    let mut idx = 1usize;
    let mut out = Vec::new();
    for _ in 0..tc {
        let n: usize = lines[idx].trim().parse().unwrap_or(0);
        idx += 1;
        let mut vals = vec![0; n];
        let mut ok = vec![false; n];
        for i in 0..n {
            let tok = lines[idx].trim();
            idx += 1;
            if tok != "null" { ok[i] = true; vals[i] = tok.parse().unwrap_or(0); }
        }
        out.push(solve(&vals, &ok).to_string());
    }
    print!("{}", out.join("\n"));
}
