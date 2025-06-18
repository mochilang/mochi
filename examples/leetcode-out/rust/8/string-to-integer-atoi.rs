fn myAtoi(s: &str) -> i32 {
    let mut i = 0;
    let mut n = s.len() as i32;
    while i < n && s.chars().nth((i) as usize).unwrap() == " " {
        i = i + 1;
    }
    let mut sign = 1;
    if i < n && (s.chars().nth((i) as usize).unwrap() == "+" || s.chars().nth((i) as usize).unwrap() == "-") {
        if s.chars().nth((i) as usize).unwrap() == "-" {
            sign = -1;
        }
        i = i + 1;
    }
    let mut digits = std::collections::HashMap::from([("0".to_string(), 0), ("1".to_string(), 1), ("2".to_string(), 2), ("3".to_string(), 3), ("4".to_string(), 4), ("5".to_string(), 5), ("6".to_string(), 6), ("7".to_string(), 7), ("8".to_string(), 8), ("9".to_string(), 9)]);
    let mut result = 0;
    while i < n {
        let mut ch = s.chars().nth((i) as usize).unwrap();
        if !(_in_map(&digits, &ch)) {
            break;
        }
        let mut d = digits[(ch) as usize];
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

fn main() {
}

fn _in_map<K: std::cmp::Eq + std::hash::Hash, V>(m: &std::collections::HashMap<K, V>, k: &K) -> bool {
    m.contains_key(k)
}
