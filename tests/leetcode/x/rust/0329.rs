use std::io::{self, Read};

fn longest_increasing_path(matrix: &[Vec<i32>]) -> i32 {
    let rows = matrix.len();
    let cols = matrix[0].len();
    let mut memo = vec![vec![0; cols]; rows];
    fn dfs(r: usize, c: usize, matrix: &[Vec<i32>], memo: &mut [Vec<i32>]) -> i32 {
        if memo[r][c] != 0 { return memo[r][c]; }
        let rows = matrix.len() as i32;
        let cols = matrix[0].len() as i32;
        let dirs = [(1,0),(-1,0),(0,1),(0,-1)];
        let mut best = 1;
        for (dr, dc) in dirs {
            let nr = r as i32 + dr; let nc = c as i32 + dc;
            if nr >= 0 && nr < rows && nc >= 0 && nc < cols && matrix[nr as usize][nc as usize] > matrix[r][c] {
                best = best.max(1 + dfs(nr as usize, nc as usize, matrix, memo));
            }
        }
        memo[r][c] = best;
        best
    }
    let mut ans = 0;
    for r in 0..rows { for c in 0..cols { ans = ans.max(dfs(r, c, matrix, &mut memo)); } }
    ans
}

fn main() {
    let mut s = String::new(); io::stdin().read_to_string(&mut s).unwrap();
    let data: Vec<i32> = s.split_whitespace().map(|x| x.parse().unwrap()).collect();
    if data.is_empty() { return; }
    let mut idx = 0; let t = data[idx] as usize; idx += 1; let mut out = Vec::new();
    for _ in 0..t {
        let rows = data[idx] as usize; let cols = data[idx+1] as usize; idx += 2;
        let mut m = vec![vec![0; cols]; rows];
        for r in 0..rows { for c in 0..cols { m[r][c] = data[idx]; idx += 1; } }
        out.push(longest_increasing_path(&m).to_string());
    }
    print!("{}", out.join("\n\n"));
}
