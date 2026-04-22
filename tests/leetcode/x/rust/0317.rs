use std::collections::VecDeque;
use std::io::{self, Read};

fn shortest_distance(grid: &[Vec<i32>]) -> i32 {
    let rows = grid.len();
    let cols = grid[0].len();
    let mut dist = vec![vec![0; cols]; rows];
    let mut reach = vec![vec![0; cols]; rows];
    let mut buildings = 0;
    for sr in 0..rows {
        for sc in 0..cols {
            if grid[sr][sc] != 1 {
                continue;
            }
            buildings += 1;
            let mut seen = vec![vec![false; cols]; rows];
            let mut q = VecDeque::new();
            q.push_back((sr, sc, 0));
            seen[sr][sc] = true;
            while let Some((r, c, d)) = q.pop_front() {
                for (dr, dc) in [(1isize, 0isize), (-1, 0), (0, 1), (0, -1)] {
                    let nr = r as isize + dr;
                    let nc = c as isize + dc;
                    if nr >= 0 && nr < rows as isize && nc >= 0 && nc < cols as isize {
                        let nr = nr as usize;
                        let nc = nc as usize;
                        if !seen[nr][nc] {
                            seen[nr][nc] = true;
                            if grid[nr][nc] == 0 {
                                dist[nr][nc] += d + 1;
                                reach[nr][nc] += 1;
                                q.push_back((nr, nc, d + 1));
                            }
                        }
                    }
                }
            }
        }
    }
    let mut ans = None;
    for r in 0..rows {
        for c in 0..cols {
            if grid[r][c] == 0 && reach[r][c] == buildings {
                ans = Some(ans.map_or(dist[r][c], |v: i32| v.min(dist[r][c])));
            }
        }
    }
    ans.unwrap_or(-1)
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let data: Vec<i32> = input.split_whitespace().filter_map(|s| s.parse().ok()).collect();
    if data.is_empty() {
        return;
    }
    let mut pos = 0usize;
    let t = data[pos] as usize;
    pos += 1;
    let mut out = Vec::new();
    for _ in 0..t {
        let rows = data[pos] as usize;
        let cols = data[pos + 1] as usize;
        pos += 2;
        let mut grid = vec![vec![0; cols]; rows];
        for row in &mut grid {
            for cell in row {
                *cell = data[pos];
                pos += 1;
            }
        }
        out.push(shortest_distance(&grid).to_string());
    }
    print!("{}", out.join("\n\n"));
}
