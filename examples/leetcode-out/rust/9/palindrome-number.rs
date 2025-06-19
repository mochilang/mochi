fn isPalindrome(x: i32) -> bool {
    if x < 0 {
        return false;
    }
    let mut s = format!("{}", x);
    let mut n = s.len() as i32;
    for i in 0..n / 2 {
        if s[((i) as usize)..((i + 1) as usize)].to_string() != s[((n - 1 - i) as usize)..((n - i) as usize)].to_string() {
            return false;
        }
    }
    return true;
}

fn main() {
}


