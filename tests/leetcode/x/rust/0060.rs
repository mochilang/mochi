use std::io::{self, Read};

fn get_permutation(n: usize, k_input: usize) -> String {
    let mut digits: Vec<String> = (1..=n).map(|x| x.to_string()).collect();
    let mut fact = vec![1usize; n + 1];
    for i in 1..=n { fact[i] = fact[i - 1] * i; }
    let mut k = k_input - 1;
    let mut out = String::new();
    for rem in (1..=n).rev() {
        let block = fact[rem - 1];
        let idx = k / block;
        k %= block;
        out.push_str(&digits.remove(idx));
    }
    out
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let lines: Vec<String> = input.lines().map(|s| s.trim().to_string()).collect();
    if lines.is_empty() || lines[0].is_empty() { return; }
    let mut idx = 0usize;
    let t: usize = lines[idx].parse().unwrap(); idx += 1;
    let mut out: Vec<String> = Vec::new();
    for _ in 0..t {
        let n: usize = lines[idx].parse().unwrap(); idx += 1;
        let k: usize = lines[idx].parse().unwrap(); idx += 1;
        out.push(get_permutation(n, k));
    }
    print!("{}", out.join("\n"));
}
