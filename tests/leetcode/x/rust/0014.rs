use std::io::{self, Read};

fn lcp(strs: &[String]) -> String {
    let mut prefix = strs[0].clone();
    while !strs.iter().all(|s| s.starts_with(&prefix)) {
        prefix.pop();
    }
    prefix
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let tokens: Vec<&str> = input.split_whitespace().collect();
    if tokens.is_empty() { return; }
    let mut idx = 0usize;
    let t: usize = tokens[idx].parse().unwrap(); idx += 1;
    let mut out = Vec::new();
    for _ in 0..t {
        let n: usize = tokens[idx].parse().unwrap(); idx += 1;
        let strs: Vec<String> = tokens[idx..idx+n].iter().map(|s| s.to_string()).collect();
        idx += n;
        out.push(format!("\"{}\"", lcp(&strs)));
    }
    print!("{}", out.join("\n"));
}
