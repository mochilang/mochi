fn isMatch(s: &str, p: &str) -> bool {
    let mut m = s.len() as i32;
    let mut n = p.len() as i32;
    let mut dp: Vec<Vec<bool>> = vec![];
    let mut i = 0;
    while i <= m {
        let mut row: Vec<bool> = vec![];
        let mut j = 0;
        while j <= n {
            row = _concat(&row, &vec![false]);
            j = j + 1;
        }
        dp = _concat(&dp, &vec![row]);
        i = i + 1;
    }
    dp[(m) as usize][(n) as usize] = true;
    let mut i2 = m;
    while i2 >= 0 {
        let mut j2 = n - 1;
        while j2 >= 0 {
            let mut first = false;
            if i2 < m {
                if (p.chars().nth((j2) as usize).unwrap() == s.chars().nth((i2) as usize).unwrap()) || (p.chars().nth((j2) as usize).unwrap() == ".".chars().nth((0) as usize).unwrap()) {
                    first = true;
                }
            }
            if j2 + 1 < n && p.chars().nth((j2 + 1) as usize).unwrap() == "*".chars().nth((0) as usize).unwrap() {
                if dp[(i2) as usize][(j2 + 2) as usize] || (first && dp[(i2 + 1) as usize][(j2) as usize]) {
                    dp[(i2) as usize][(j2) as usize] = true;
                } else {
                    dp[(i2) as usize][(j2) as usize] = false;
                }
            } else {
                if first && dp[(i2 + 1) as usize][(j2 + 1) as usize] {
                    dp[(i2) as usize][(j2) as usize] = true;
                } else {
                    dp[(i2) as usize][(j2) as usize] = false;
                }
            }
            j2 = j2 - 1;
        }
        i2 = i2 - 1;
    }
    return dp[(0) as usize][(0) as usize];
}

fn main() {
}

fn _concat<T: Clone>(a: &[T], b: &[T]) -> Vec<T> {
    let mut res = Vec::with_capacity(a.len() + b.len());
    res.extend_from_slice(a);
    res.extend_from_slice(b);
    res
}

