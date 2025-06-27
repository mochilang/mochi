fn expand(s: &str, left: i64, right: i64) -> i64 {
    let mut l = left;
    let mut r = right;
    let mut n = s.len() as i64;
    while l >= 0 && r < n {
        if {
            let s = &s;
            let mut idx = l;
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
            let mut idx = r;
            let chars: Vec<char> = s.chars().collect();
            if idx < 0 {
                idx += chars.len() as i64;
            }
            if idx < 0 || idx >= chars.len() as i64 {
                panic!("index out of range");
            }
            chars[idx as usize].to_string()
        } {
            break;
        }
        l = l - 1;
        r = r + 1;
    }
    return r - l - 1;
}

fn longestPalindrome(s: &str) -> String {
    if s.len() as i64 <= 1 {
        return s.to_string();
    }
    let mut start = 0;
    let mut end = 0;
    let mut n = s.len() as i64;
    for i in 0..n {
        let mut len1 = expand(&s, i, i);
        let mut len2 = expand(&s, i, i + 1);
        let mut l = len1;
        if len2 > len1 {
            l = len2;
        }
        if l > (end - start) {
            start = i - ((l - 1) / 2);
            end = i + (l / 2);
        }
    }
    return _slice_string(s, start, end + 1).to_string();
}

fn test_example_1() {
    let mut ans = longestPalindrome("babad");
    expect(ans == "bab" || ans == "aba");
}

fn test_example_2() {
    expect(longestPalindrome("cbbd") == "bb");
}

fn test_single_char() {
    expect(longestPalindrome("a") == "a");
}

fn test_two_chars() {
    let mut ans = longestPalindrome("ac");
    expect(ans == "a" || ans == "c");
}

fn main() {
    test_example_1();
    test_example_2();
    test_single_char();
    test_two_chars();
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
