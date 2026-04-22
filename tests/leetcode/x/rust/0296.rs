use std::io::{self, Read};

fn min_total_distance(grid: &[Vec<i32>]) -> i32 {
    let mut rows = Vec::new();
    let mut cols = Vec::new();
    for i in 0..grid.len() {
        for j in 0..grid[i].len() {
            if grid[i][j] == 1 {
                rows.push(i as i32);
            }
        }
    }
    for j in 0..grid[0].len() {
        for i in 0..grid.len() {
            if grid[i][j] == 1 {
                cols.push(j as i32);
            }
        }
    }
    let mr = rows[rows.len() / 2];
    let mc = cols[cols.len() / 2];
    rows.into_iter().map(|r| (r - mr).abs()).sum::<i32>() + cols.into_iter().map(|c| (c - mc).abs()).sum::<i32>()
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
    let mut out = Vec::new();
    for _ in 0..t {
        let r = data[idx] as usize;
        let c = data[idx + 1] as usize;
        idx += 2;
        let mut grid = vec![vec![0; c]; r];
        for row in &mut grid {
            for cell in row {
                *cell = data[idx];
                idx += 1;
            }
        }
        out.push(min_total_distance(&grid).to_string());
    }
    print!("{}", out.join("\n\n"));
}
