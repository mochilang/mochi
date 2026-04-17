use std::io::{self, Read};

fn convert(s: &str, num_rows: usize) -> String {
    let chars: Vec<char> = s.chars().collect();
    if num_rows <= 1 || num_rows >= chars.len() {
        return s.to_string();
    }
    let cycle = 2 * num_rows - 2;
    let mut out = String::new();
    for row in 0..num_rows {
        let mut i = row;
        while i < chars.len() {
            out.push(chars[i]);
            let diag = i + cycle - 2 * row;
            if row > 0 && row < num_rows - 1 && diag < chars.len() {
                out.push(chars[diag]);
            }
            i += cycle;
        }
    }
    out
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    if input.is_empty() {
        return;
    }
    let lines: Vec<&str> = input.split('\n').collect();
    let t: usize = lines.first().unwrap_or(&"").trim().parse().unwrap_or(0);
    let mut out = Vec::new();
    let mut idx = 1usize;
    for _ in 0..t {
        let s = lines.get(idx).copied().unwrap_or("").trim_end_matches('\r');
        idx += 1;
        let num_rows: usize = lines.get(idx).copied().unwrap_or("1").trim().parse().unwrap_or(1);
        idx += 1;
        out.push(convert(s, num_rows));
    }
    print!("{}", out.join("\n"));
}
