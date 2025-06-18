fn lengthOfLongestSubstring(s: &str) -> i32 {
    let mut n = s.len() as i32;
    let mut start = 0;
    let mut best = 0;
    let mut i = 0;
    while i < n {
        let mut j = start;
        while j < i {
            if s.chars().nth((j) as usize).unwrap() == s.chars().nth((i) as usize).unwrap() {
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

fn main() {
}

