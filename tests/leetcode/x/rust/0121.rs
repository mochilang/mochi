use std::io::{self, Read};

fn max_profit(prices: &[i32]) -> i32 {
    if prices.is_empty() {
        return 0;
    }
    let mut min_price = prices[0];
    let mut best = 0i32;
    for &p in &prices[1..] {
        best = best.max(p - min_price);
        min_price = min_price.min(p);
    }
    best
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    if input.is_empty() {
        return;
    }
    let lines: Vec<&str> = input.split('\n').collect();
    let t: usize = lines[0].trim().parse().unwrap_or(0);
    let mut idx = 1usize;
    let mut out = Vec::new();
    for _ in 0..t {
        let n: usize = lines.get(idx).unwrap_or(&"0").trim().parse().unwrap_or(0);
        idx += 1;
        let mut prices = Vec::new();
        for _ in 0..n {
            prices.push(lines.get(idx).unwrap_or(&"0").trim().parse().unwrap_or(0));
            idx += 1;
        }
        out.push(max_profit(&prices).to_string());
    }
    print!("{}", out.join("\n"));
}
