use std::collections::HashMap;
use std::io::{self, Read};

fn find(x: i32, parent: &mut HashMap<i32, i32>) -> i32 {
    let mut x = x;
    while parent[&x] != x {
        let px = parent[&x];
        let ppx = parent[&px];
        parent.insert(x, ppx);
        x = parent[&x];
    }
    x
}

fn union(a: i32, b: i32, parent: &mut HashMap<i32, i32>, rank: &mut HashMap<i32, i32>) -> bool {
    let mut ra = find(a, parent);
    let mut rb = find(b, parent);
    if ra == rb {
        return false;
    }
    if rank[&ra] < rank[&rb] {
        std::mem::swap(&mut ra, &mut rb);
    }
    parent.insert(rb, ra);
    if rank[&ra] == rank[&rb] {
        let v = rank[&ra] + 1;
        rank.insert(ra, v);
    }
    true
}

fn solve(m: i32, n: i32, positions: &[(i32, i32)]) -> Vec<i32> {
    let mut parent = HashMap::new();
    let mut rank = HashMap::new();
    let mut ans = Vec::new();
    let mut count = 0;
    for &(r, c) in positions {
        let idx = r * n + c;
        if parent.contains_key(&idx) {
            ans.push(count);
            continue;
        }
        parent.insert(idx, idx);
        rank.insert(idx, 0);
        count += 1;
        for (dr, dc) in [(1, 0), (-1, 0), (0, 1), (0, -1)] {
            let nr = r + dr;
            let nc = c + dc;
            if nr >= 0 && nr < m && nc >= 0 && nc < n {
                let nei = nr * n + nc;
                if parent.contains_key(&nei) && union(idx, nei, &mut parent, &mut rank) {
                    count -= 1;
                }
            }
        }
        ans.push(count);
    }
    ans
}

fn fmt_list(a: &[i32]) -> String {
    format!("[{}]", a.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(","))
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let data: Vec<i32> = input.split_whitespace().filter_map(|s| s.parse().ok()).collect();
    if data.is_empty() {
        return;
    }
    let mut idx = 0usize;
    let t = data[idx] as usize;
    idx += 1;
    let mut blocks = Vec::new();
    for _ in 0..t {
        let m = data[idx];
        let n = data[idx + 1];
        let k = data[idx + 2] as usize;
        idx += 3;
        let mut positions = Vec::new();
        for _ in 0..k {
            positions.push((data[idx], data[idx + 1]));
            idx += 2;
        }
        blocks.push(fmt_list(&solve(m, n, &positions)));
    }
    print!("{}", blocks.join("\n\n"));
}
