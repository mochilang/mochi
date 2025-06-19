fn isPalindrome(x: i32) -> bool {
    if x < 0 {
        return false;
    }
    let mut s: String = format!("{}", x);
    let mut n = s.len() as i32;
    for i in 0..n / 2 {
        if s.chars().nth((i) as usize).unwrap() != s.chars().nth((n - 1 - i) as usize).unwrap() {
            return false;
        }
    }
    return true;
}

fn main() {
}

