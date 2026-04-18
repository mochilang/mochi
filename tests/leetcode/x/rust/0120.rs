use std::io::{self, Read};

fn solve(tri: Vec<Vec<i32>>) -> i32 {
    let mut dp = tri.last().unwrap().clone();
    for i in (0..tri.len() - 1).rev() {
        for j in 0..=i {
            dp[j] = tri[i][j] + dp[j].min(dp[j + 1]);
        }
    }
    dp[0]
}

fn main() {
    let mut s = String::new();
    io::stdin().read_to_string(&mut s).unwrap();
    let mut it = s.split_whitespace();
    let t: usize = match it.next() { Some(v) => v.parse().unwrap(), None => return };
    let mut out = Vec::new();
    for _ in 0..t {
        let rows: usize = it.next().unwrap().parse().unwrap();
        let mut tri = Vec::new();
        for r in 1..=rows {
            let mut row = Vec::new();
            for _ in 0..r { row.push(it.next().unwrap().parse::<i32>().unwrap()); }
            tri.push(row);
        }
        out.push(solve(tri).to_string());
    }
    print!("{}", out.join("\n"));
}
