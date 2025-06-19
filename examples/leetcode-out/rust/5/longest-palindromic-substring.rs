fn expand(s: &str, left: i32, right: i32) -> i32 {
    let mut l = left;
    let mut r = right;
    let mut n = s.len() as i32;
    while l >= 0 && r < n {
        if s.chars().nth((l) as usize).unwrap() != s.chars().nth((r) as usize).unwrap() {
            break;
        }
        l = l - 1;
        r = r + 1;
    }
    return r - l - 1;
}

fn longestPalindrome(s: &str) -> String {
    if s.len() as i32 <= 1 {
        return s.to_string();
    }
    let mut start = 0;
    let mut end = 0;
    let mut n = s.len() as i32;
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
    return s[((start) as usize)..((end + 1) as usize)].to_string().to_string();
}

fn main() {
}

