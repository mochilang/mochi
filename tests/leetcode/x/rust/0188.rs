use std::io::{self, Read};

fn solve(k: usize, prices: Vec<i64>) -> i64 {
    let n = prices.len();
    if k >= n / 2 {
        let mut best = 0;
        for i in 1..n {
            if prices[i] > prices[i - 1] {
                best += prices[i] - prices[i - 1];
            }
        }
        return best;
    }
    let neg_inf = -(1_i64 << 60);
    let mut buy = vec![neg_inf; k + 1];
    let mut sell = vec![0_i64; k + 1];
    for price in prices {
        for t in 1..=k {
            buy[t] = buy[t].max(sell[t - 1] - price);
            sell[t] = sell[t].max(buy[t] + price);
        }
    }
    sell[k]
}

fn main() {
    let mut s = String::new();
    io::stdin().read_to_string(&mut s).unwrap();
    let mut it = s.split_whitespace();
    let t: usize = match it.next() { Some(v) => v.parse().unwrap(), None => return };
    let mut out = Vec::new();
    for _ in 0..t {
        let k: usize = it.next().unwrap().parse().unwrap();
        let n: usize = it.next().unwrap().parse().unwrap();
        let mut prices = Vec::new();
        for _ in 0..n { prices.push(it.next().unwrap().parse::<i64>().unwrap()); }
        out.push(solve(k, prices).to_string());
    }
    print!("{}", out.join("\n"));
}
