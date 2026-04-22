use std::io::{self, Read};

const LESS20: [&str; 20] = ["", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine",
    "Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen"];
const TENS: [&str; 10] = ["", "", "Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety"];
const THOUSANDS: [&str; 4] = ["", "Thousand", "Million", "Billion"];

fn helper(n: i32) -> String {
    if n == 0 {
        String::new()
    } else if n < 20 {
        LESS20[n as usize].to_string()
    } else if n < 100 {
        if n % 10 == 0 {
            TENS[(n / 10) as usize].to_string()
        } else {
            format!("{} {}", TENS[(n / 10) as usize], helper(n % 10))
        }
    } else if n % 100 == 0 {
        format!("{} Hundred", LESS20[(n / 100) as usize])
    } else {
        format!("{} Hundred {}", LESS20[(n / 100) as usize], helper(n % 100))
    }
}

fn solve(mut num: i32) -> String {
    if num == 0 {
        return "Zero".to_string();
    }
    let mut parts: Vec<String> = Vec::new();
    let mut idx = 0usize;
    while num > 0 {
        let chunk = num % 1000;
        if chunk != 0 {
            let mut words = helper(chunk);
            if !THOUSANDS[idx].is_empty() {
                words.push(' ');
                words.push_str(THOUSANDS[idx]);
            }
            parts.push(words);
        }
        num /= 1000;
        idx += 1;
    }
    parts.reverse();
    parts.join(" ")
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let lines: Vec<&str> = input.lines().collect();
    if lines.is_empty() {
        return;
    }
    let t: usize = lines[0].trim().parse().unwrap();
    let mut out = Vec::with_capacity(t);
    for i in 0..t {
        let num: i32 = lines[i + 1].trim().parse().unwrap();
        out.push(solve(num));
    }
    print!("{}", out.join("\n"));
}
