fn digit(ch: &str) -> i32 {
    if ch == "0" {
        return 0;
    }
    if ch == "1" {
        return 1;
    }
    if ch == "2" {
        return 2;
    }
    if ch == "3" {
        return 3;
    }
    if ch == "4" {
        return 4;
    }
    if ch == "5" {
        return 5;
    }
    if ch == "6" {
        return 6;
    }
    if ch == "7" {
        return 7;
    }
    if ch == "8" {
        return 8;
    }
    if ch == "9" {
        return 9;
    }
    return -1;
}

fn myAtoi(s: &str) -> i32 {
    let mut i = 0;
    let mut n = s.len() as i32;
    while i < n && s.chars().nth((i) as usize).unwrap() == " ".chars().nth((0) as usize).unwrap() {
        i = i + 1;
    }
    let mut sign = 1;
    if i < n && (s.chars().nth((i) as usize).unwrap() == "+".chars().nth((0) as usize).unwrap() || s.chars().nth((i) as usize).unwrap() == "-".chars().nth((0) as usize).unwrap()) {
        if s.chars().nth((i) as usize).unwrap() == "-".chars().nth((0) as usize).unwrap() {
            sign = -1;
        }
        i = i + 1;
    }
    let mut result = 0;
    while i < n {
        let mut ch = s[((i) as usize)..((i + 1) as usize)].to_string();
        let mut d = digit(&ch);
        if d < 0 {
            break;
        }
        result = result * 10 + d;
        i = i + 1;
    }
    result = result * sign;
    if result > 2147483647 {
        return 2147483647;
    }
    if result < (-2147483648) {
        return -2147483648;
    }
    return result;
}

fn main() {
}

