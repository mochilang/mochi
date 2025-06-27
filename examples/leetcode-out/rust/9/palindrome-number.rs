fn isPalindrome(x: i64) -> bool {
    if x < 0 {
        return false;
    }
    let mut s: String = format!("{}", x);
    let mut n = s.len() as i64;
    for i in 0..n / 2 {
        if {
            let s = &s;
            let mut idx = i;
            let chars: Vec<char> = s.chars().collect();
            if idx < 0 {
                idx += chars.len() as i64;
            }
            if idx < 0 || idx >= chars.len() as i64 {
                panic!("index out of range");
            }
            chars[idx as usize].to_string()
        } != {
            let s = &s;
            let mut idx = n - 1 - i;
            let chars: Vec<char> = s.chars().collect();
            if idx < 0 {
                idx += chars.len() as i64;
            }
            if idx < 0 || idx >= chars.len() as i64 {
                panic!("index out of range");
            }
            chars[idx as usize].to_string()
        } {
            return false;
        }
    }
    return true;
}

fn test_example_1() {
    expect(isPalindrome(121) == true);
}

fn test_example_2() {
    expect(isPalindrome(-121) == false);
}

fn test_example_3() {
    expect(isPalindrome(10) == false);
}

fn test_zero() {
    expect(isPalindrome(0) == true);
}

fn main() {
    test_example_1();
    test_example_2();
    test_example_3();
    test_zero();
}

fn expect(cond: bool) {
    if !cond {
        panic!("expect failed");
    }
}
