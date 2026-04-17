use std::io::{self, Read};

fn reverse_int(mut x: i32) -> i32 {
    let mut ans = 0i32;
    while x != 0 {
        let digit = x % 10;
        x /= 10;
        if ans > i32::MAX / 10 || (ans == i32::MAX / 10 && digit > 7) {
            return 0;
        }
        if ans < i32::MIN / 10 || (ans == i32::MIN / 10 && digit < -8) {
            return 0;
        }
        ans = ans * 10 + digit;
    }
    ans
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
    for i in 0..t {
        let x: i32 = lines.get(i + 1).copied().unwrap_or("0").trim().parse().unwrap_or(0);
        out.push(reverse_int(x).to_string());
    }
    print!("{}", out.join("\n"));
}
