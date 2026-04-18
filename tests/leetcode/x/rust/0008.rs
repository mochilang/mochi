use std::io::{self, Read};

fn my_atoi(s: &str) -> i32 {
    let bytes = s.as_bytes();
    let mut i = 0usize;
    while i < bytes.len() && bytes[i] == b' ' { i += 1; }
    let mut sign = 1;
    if i < bytes.len() && (bytes[i] == b'+' || bytes[i] == b'-') {
        if bytes[i] == b'-' { sign = -1; }
        i += 1;
    }
    let mut ans = 0i32;
    let limit = if sign > 0 { 7 } else { 8 };
    while i < bytes.len() && bytes[i].is_ascii_digit() {
        let digit = (bytes[i] - b'0') as i32;
        if ans > 214748364 || (ans == 214748364 && digit > limit) {
            return if sign > 0 { i32::MAX } else { i32::MIN };
        }
        ans = ans * 10 + digit;
        i += 1;
    }
    sign * ans
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    if input.is_empty() { return; }
    let lines: Vec<&str> = input.split('\n').collect();
    let t: usize = lines.first().unwrap_or(&"").trim().parse().unwrap_or(0);
    let mut out = Vec::new();
    for i in 0..t {
        let s = lines.get(i + 1).copied().unwrap_or("").trim_end_matches('\r');
        out.push(my_atoi(s).to_string());
    }
    print!("{}", out.join("\n"));
}
