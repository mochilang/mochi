use std::io::{self, Read};

fn max_profit(prices: &[i32]) -> i32 {
    let mut buy1 = -1_000_000_000;
    let mut sell1 = 0;
    let mut buy2 = -1_000_000_000;
    let mut sell2 = 0;
    for &p in prices {
        buy1 = buy1.max(-p);
        sell1 = sell1.max(buy1 + p);
        buy2 = buy2.max(sell1 - p);
        sell2 = sell2.max(buy2 + p);
    }
    sell2
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
