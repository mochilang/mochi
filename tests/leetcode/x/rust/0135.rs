use std::io::{self, Read};

fn candy(ratings: Vec<i32>) -> i32 {
    let n = ratings.len();
    let mut candies = vec![1i32; n];
    for i in 1..n {
        if ratings[i] > ratings[i - 1] {
            candies[i] = candies[i - 1] + 1;
        }
    }
    for i in (0..n - 1).rev() {
        if ratings[i] > ratings[i + 1] {
            candies[i] = candies[i].max(candies[i + 1] + 1);
        }
    }
    candies.iter().sum()
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
        let n: usize = lines[idx].trim().parse().unwrap_or(0);
        idx += 1;
        let mut ratings = Vec::new();
        for _ in 0..n {
            ratings.push(lines[idx].trim().parse().unwrap_or(0));
            idx += 1;
        }
        out.push(candy(ratings).to_string());
    }
    print!("{}", out.join("\n\n"));
}
