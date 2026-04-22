use std::io::{self, Read};

fn calculate(expr: &str) -> i32 {
    let mut result = 0i32;
    let mut number = 0i32;
    let mut sign = 1i32;
    let mut stack: Vec<i32> = Vec::new();
    for ch in expr.chars() {
        if ch.is_ascii_digit() {
            number = number * 10 + (ch as i32 - '0' as i32);
        } else if ch == '+' || ch == '-' {
            result += sign * number;
            number = 0;
            sign = if ch == '+' { 1 } else { -1 };
        } else if ch == '(' {
            stack.push(result);
            stack.push(sign);
            result = 0;
            number = 0;
            sign = 1;
        } else if ch == ')' {
            result += sign * number;
            number = 0;
            let prev_sign = stack.pop().unwrap();
            let prev_result = stack.pop().unwrap();
            result = prev_result + prev_sign * result;
        }
    }
    result + sign * number
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let mut lines = input.lines();
    let Some(first) = lines.next() else { return; };
    let t: usize = first.trim().parse().unwrap();
    let mut out: Vec<String> = Vec::with_capacity(t);
    for expr in lines.take(t) {
        out.push(calculate(expr).to_string());
    }
    print!("{}", out.join("\n"));
}
