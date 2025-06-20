fn strStr(haystack: &str, needle: &str) -> i32 {
    let mut n = haystack.len() as i32;
    let mut m = needle.len() as i32;
    if m == 0 {
        return 0;
    }
    if m > n {
        return -1;
    }
    for i in 0..n - m + 1 {
        let mut j = 0;
        while j < m {
            if haystack.chars().nth((i + j) as usize).unwrap() != needle.chars().nth((j) as usize).unwrap() {
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

fn main() {
}

