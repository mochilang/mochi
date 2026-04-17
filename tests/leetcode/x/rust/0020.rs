use std::io::{self, Read};

fn is_valid(s: &str) -> bool {
    let mut stack: Vec<char> = Vec::new();
    for ch in s.chars() {
        if ch == '(' || ch == '[' || ch == '{' {
            stack.push(ch);
        } else {
            let open = match stack.pop() {
                Some(v) => v,
                None => return false,
            };
            if (ch == ')' && open != '(') ||
               (ch == ']' && open != '[') ||
               (ch == '}' && open != '{') {
                return false;
            }
        }
    }
    stack.is_empty()
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let tokens: Vec<&str> = input.split_whitespace().collect();
    if tokens.is_empty() {
        return;
    }
    let t: usize = tokens[0].parse().unwrap();
    let out: Vec<&str> = tokens[1..1 + t]
        .iter()
        .map(|s| if is_valid(s) { "true" } else { "false" })
        .collect();
    print!("{}", out.join("\n"));
}
