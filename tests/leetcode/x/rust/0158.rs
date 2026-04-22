use std::io::{self, Read};

fn quote(s: &str) -> String {
    format!("\"{}\"", s)
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let lines: Vec<&str> = input.lines().collect();
    if lines.is_empty() {
        return;
    }
    let tc: usize = lines[0].trim().parse().unwrap_or(0);
    let mut idx = 1usize;
    let mut out = Vec::new();
    for _ in 0..tc {
        let data = lines[idx].to_string();
        idx += 1;
        let q: usize = lines[idx].trim().parse().unwrap_or(0);
        idx += 1;
        let mut pos = 0usize;
        let mut block = vec![q.to_string()];
        for _ in 0..q {
            let n: usize = lines[idx].trim().parse().unwrap_or(0);
            idx += 1;
            let end = (pos + n).min(data.len());
            let part = &data[pos..end];
            pos = end;
            block.push(quote(part));
        }
        out.push(block.join("\n"));
    }
    print!("{}", out.join("\n\n"));
}
