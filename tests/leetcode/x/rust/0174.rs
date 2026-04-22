use std::io::{self, Read};

fn solve(dungeon: Vec<Vec<i64>>) -> i64 {
    let cols = dungeon[0].len();
    let inf = i64::MAX / 4;
    let mut dp = vec![inf; cols + 1];
    dp[cols - 1] = 1;
    for i in (0..dungeon.len()).rev() {
        for j in (0..cols).rev() {
            let need = dp[j].min(dp[j + 1]) - dungeon[i][j];
            dp[j] = if need <= 1 { 1 } else { need };
        }
    }
    dp[0]
}

fn main() {
    let mut s = String::new();
    io::stdin().read_to_string(&mut s).unwrap();
    let mut it = s.split_whitespace();
    let t: usize = match it.next() {
        Some(v) => v.parse().unwrap(),
        None => return,
    };
    let mut out = Vec::new();
    for _ in 0..t {
        let rows: usize = it.next().unwrap().parse().unwrap();
        let cols: usize = it.next().unwrap().parse().unwrap();
        let mut dungeon = Vec::new();
        for _ in 0..rows {
            let mut row = Vec::new();
            for _ in 0..cols {
                row.push(it.next().unwrap().parse::<i64>().unwrap());
            }
            dungeon.push(row);
        }
        out.push(solve(dungeon).to_string());
    }
    print!("{}", out.join("\n"));
}
