fn strStr(haystack: &str, needle: &str) -> i64 {
    let mut n = haystack.len() as i64;
    let mut m = needle.len() as i64;
    if m == 0 {
        return 0;
    }
    if m > n {
        return -1;
    }
    for i in 0..n - m + 1 {
        let mut j = 0;
        while j < m {
            if {
                let s = &haystack;
                let mut idx = i + j;
                let chars: Vec<char> = s.chars().collect();
                if idx < 0 {
                    idx += chars.len() as i64;
                }
                if idx < 0 || idx >= chars.len() as i64 {
                    panic!("index out of range");
                }
                chars[idx as usize].to_string()
            } != {
                let s = &needle;
                let mut idx = j;
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
            j = j + 1;
        }
        if j == m {
            return i;
        }
    }
    return -1;
}

fn test_example_1() {
    expect(strStr("sadbutsad", "sad") == 0);
}

fn test_example_2() {
    expect(strStr("leetcode", "leeto") == (-1));
}

fn test_empty_needle() {
    expect(strStr("abc", "") == 0);
}

fn test_needle_at_end() {
    expect(strStr("hello", "lo") == 3);
}

fn main() {
    test_example_1();
    test_example_2();
    test_empty_needle();
    test_needle_at_end();
}

fn expect(cond: bool) {
    if !cond {
        panic!("expect failed");
    }
}
