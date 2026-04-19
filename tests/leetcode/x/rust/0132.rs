use std::io::{self, Read};

fn min_cut(s: String) -> i32 {
    let chars: Vec<char> = s.chars().collect();
    let n = chars.len();
    let mut pal = vec![vec![false; n]; n];
    let mut cuts = vec![0i32; n];
    for end in 0..n {
        cuts[end] = end as i32;
        for start in 0..=end {
            if chars[start] == chars[end] && (end - start <= 2 || pal[start + 1][end - 1]) {
                pal[start][end] = true;
                if start == 0 {
                    cuts[end] = 0;
                } else {
                    cuts[end] = cuts[end].min(cuts[start - 1] + 1);
                }
            }
        }
    }
    cuts[n - 1]
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let lines: Vec<&str> = input.lines().collect();
    if lines.is_empty() {
        return;
    }
    let tc: usize = lines[0].trim().parse().unwrap_or(0);
    let mut out = Vec::new();
    for i in 1..=tc {
        out.push(min_cut(lines[i].to_string()).to_string());
    }
    print!("{}", out.join("\n\n"));
}
