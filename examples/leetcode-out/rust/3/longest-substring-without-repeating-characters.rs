fn lengthOfLongestSubstring(s: &str) -> i64 {
    let mut n = s.len() as i64;
    let mut start = 0;
    let mut best = 0;
    let mut i = 0;
    while i < n {
        let mut j = start;
        while j < i {
            if {
                let s = &s;
                let mut idx = j;
                let chars: Vec<char> = s.chars().collect();
                if idx < 0 {
                    idx += chars.len() as i64;
                }
                if idx < 0 || idx >= chars.len() as i64 {
                    panic!("index out of range");
                }
                chars[idx as usize].to_string()
            } == {
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
            } {
                start = j + 1;
                break;
            }
            j = j + 1;
        }
        let mut length = i - start + 1;
        if length > best {
            best = length;
        }
        i = i + 1;
    }
    return best;
}

fn test_example_1() {
    expect(lengthOfLongestSubstring("abcabcbb") == 3);
}

fn test_example_2() {
    expect(lengthOfLongestSubstring("bbbbb") == 1);
}

fn test_example_3() {
    expect(lengthOfLongestSubstring("pwwkew") == 3);
}

fn test_empty_string() {
    expect(lengthOfLongestSubstring("") == 0);
}

fn main() {
    test_example_1();
    test_example_2();
    test_example_3();
    test_empty_string();
}

fn expect(cond: bool) {
    if !cond {
        panic!("expect failed");
    }
}
