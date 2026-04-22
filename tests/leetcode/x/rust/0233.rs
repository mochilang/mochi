use std::io::{self, Read};

fn count_digit_one(n: i64) -> i64 {
    let mut total = 0i64;
    let mut m = 1i64;
    while m <= n {
        let high = n / (m * 10);
        let cur = (n / m) % 10;
        let low = n % m;
        if cur == 0 {
            total += high * m;
        } else if cur == 1 {
            total += high * m + low + 1;
        } else {
            total += (high + 1) * m;
        }
        m *= 10;
    }
    total
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let mut lines = input.lines();
    let Some(first) = lines.next() else { return; };
    let t: usize = first.trim().parse().unwrap();
    let mut out = Vec::with_capacity(t);
    for line in lines.take(t) {
        let n: i64 = line.trim().parse().unwrap();
        out.push(count_digit_one(n).to_string());
    }
    print!("{}", out.join("\n"));
}
