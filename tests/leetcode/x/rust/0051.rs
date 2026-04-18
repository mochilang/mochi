use std::io::{self, Read};

fn solve(n: usize) -> Vec<Vec<String>> {
    let mut cols = vec![false; n];
    let mut d1 = vec![false; 2 * n];
    let mut d2 = vec![false; 2 * n];
    let mut board = vec![vec!['.'; n]; n];
    let mut res: Vec<Vec<String>> = Vec::new();
    fn dfs(r: usize, n: usize, cols: &mut [bool], d1: &mut [bool], d2: &mut [bool], board: &mut Vec<Vec<char>>, res: &mut Vec<Vec<String>>) {
        if r == n { res.push(board.iter().map(|row| row.iter().collect()).collect()); return; }
        for c in 0..n { let a = r + c; let b = r as isize - c as isize + n as isize - 1; let b = b as usize; if cols[c] || d1[a] || d2[b] { continue; } cols[c]=true; d1[a]=true; d2[b]=true; board[r][c]='Q'; dfs(r+1,n,cols,d1,d2,board,res); board[r][c]='.'; cols[c]=false; d1[a]=false; d2[b]=false; }
    }
    dfs(0, n, &mut cols, &mut d1, &mut d2, &mut board, &mut res); res
}

fn main(){ let mut input=String::new(); io::stdin().read_to_string(&mut input).unwrap(); let lines: Vec<String>=input.lines().map(|s| s.trim().to_string()).collect(); if lines.is_empty()||lines[0].is_empty(){return;} let mut idx=0usize; let t:usize=lines[idx].parse().unwrap(); idx+=1; let mut out:Vec<String>=Vec::new(); for tc in 0..t { let n:usize=lines[idx].parse().unwrap(); idx+=1; let sols=solve(n); out.push(sols.len().to_string()); for si in 0..sols.len(){ for row in &sols[si]{ out.push(row.clone()); } if si+1<sols.len(){ out.push("-".to_string()); } } if tc+1<t{ out.push("=".to_string()); } } print!("{}", out.join("
")); }
