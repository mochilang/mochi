fn digit(ch: &str) -> i64 {
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

fn myAtoi(s: &str) -> i64 {
    let mut i = 0;
    let mut n = s.len() as i64;
    while i < n && {
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
    } == {
        let s = &" ";
        let mut idx = 0;
        let chars: Vec<char> = s.chars().collect();
        if idx < 0 {
            idx += chars.len() as i64;
        }
        if idx < 0 || idx >= chars.len() as i64 {
            panic!("index out of range");
        }
        chars[idx as usize].to_string()
    } {
        i = i + 1;
    }
    let mut sign = 1;
    if i < n
        && ({
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
        } == {
            let s = &"+";
            let mut idx = 0;
            let chars: Vec<char> = s.chars().collect();
            if idx < 0 {
                idx += chars.len() as i64;
            }
            if idx < 0 || idx >= chars.len() as i64 {
                panic!("index out of range");
            }
            chars[idx as usize].to_string()
        } || {
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
        } == {
            let s = &"-";
            let mut idx = 0;
            let chars: Vec<char> = s.chars().collect();
            if idx < 0 {
                idx += chars.len() as i64;
            }
            if idx < 0 || idx >= chars.len() as i64 {
                panic!("index out of range");
            }
            chars[idx as usize].to_string()
        })
    {
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
        } == {
            let s = &"-";
            let mut idx = 0;
            let chars: Vec<char> = s.chars().collect();
            if idx < 0 {
                idx += chars.len() as i64;
            }
            if idx < 0 || idx >= chars.len() as i64 {
                panic!("index out of range");
            }
            chars[idx as usize].to_string()
        } {
            sign = -1;
        }
        i = i + 1;
    }
    let mut result = 0;
    while i < n {
        let mut ch = _slice_string(s, i, i + 1);
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

fn test_example_1() {
    expect(myAtoi("42") == 42);
}

fn test_example_2() {
    expect(myAtoi("   -42") == (-42));
}

fn test_example_3() {
    expect(myAtoi("4193 with words") == 4193);
}

fn test_example_4() {
    expect(myAtoi("words and 987") == 0);
}

fn test_example_5() {
    expect(myAtoi("-91283472332") == (-2147483648));
}

fn main() {
    test_example_1();
    test_example_2();
    test_example_3();
    test_example_4();
    test_example_5();
}

fn _slice_string(s: &str, start: i64, end: i64) -> String {
    let mut sidx = start;
    let mut eidx = end;
    let chars: Vec<char> = s.chars().collect();
    let n = chars.len() as i64;
    if sidx < 0 {
        sidx += n;
    }
    if eidx < 0 {
        eidx += n;
    }
    if sidx < 0 {
        sidx = 0;
    }
    if eidx > n {
        eidx = n;
    }
    if eidx < sidx {
        eidx = sidx;
    }
    chars[sidx as usize..eidx as usize].iter().collect()
}
fn expect(cond: bool) {
    if !cond {
        panic!("expect failed");
    }
}
