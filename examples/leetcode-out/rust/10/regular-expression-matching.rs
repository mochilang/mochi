fn isMatch(s: &str, p: &str) -> bool {
    let mut m = s.len() as i64;
    let mut n = p.len() as i64;
    let mut dp: Vec<Vec<bool>> = vec![];
    let mut i = 0;
    while i <= m {
        let mut row: Vec<bool> = vec![];
        let mut j = 0;
        while j <= n {
            row = {
                let a = &row;
                let b = &vec![false];
                let mut res = Vec::with_capacity(a.len() + b.len());
                res.extend_from_slice(a);
                res.extend_from_slice(b);
                res
            };
            j = j + 1;
        }
        dp = {
            let a = &dp;
            let b = &vec![row];
            let mut res = Vec::with_capacity(a.len() + b.len());
            res.extend_from_slice(a);
            res.extend_from_slice(b);
            res
        };
        i = i + 1;
    }
    dp[m as usize][n as usize] = true;
    let mut i2 = m;
    while i2 >= 0 {
        let mut j2 = n - 1;
        while j2 >= 0 {
            let mut first = false;
            if i2 < m {
                if ({
                    let s = &p;
                    let mut idx = j2;
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
                    let mut idx = i2;
                    let chars: Vec<char> = s.chars().collect();
                    if idx < 0 {
                        idx += chars.len() as i64;
                    }
                    if idx < 0 || idx >= chars.len() as i64 {
                        panic!("index out of range");
                    }
                    chars[idx as usize].to_string()
                }) || ({
                    let s = &p;
                    let mut idx = j2;
                    let chars: Vec<char> = s.chars().collect();
                    if idx < 0 {
                        idx += chars.len() as i64;
                    }
                    if idx < 0 || idx >= chars.len() as i64 {
                        panic!("index out of range");
                    }
                    chars[idx as usize].to_string()
                } == ".")
                {
                    first = true;
                }
            }
            let mut star = false;
            if j2 + 1 < n {
                if {
                    let s = &p;
                    let mut idx = j2 + 1;
                    let chars: Vec<char> = s.chars().collect();
                    if idx < 0 {
                        idx += chars.len() as i64;
                    }
                    if idx < 0 || idx >= chars.len() as i64 {
                        panic!("index out of range");
                    }
                    chars[idx as usize].to_string()
                } == "*"
                {
                    star = true;
                }
            }
            if star {
                let mut ok = false;
                if dp[i2 as usize][(j2 + 2) as usize] {
                    ok = true;
                } else {
                    if first {
                        if dp[(i2 + 1) as usize][j2 as usize] {
                            ok = true;
                        }
                    }
                }
                dp[i2 as usize][j2 as usize] = ok;
            } else {
                let mut ok = false;
                if first {
                    if dp[(i2 + 1) as usize][(j2 + 1) as usize] {
                        ok = true;
                    }
                }
                dp[i2 as usize][j2 as usize] = ok;
            }
            j2 = j2 - 1;
        }
        i2 = i2 - 1;
    }
    return dp[(0) as usize][(0) as usize];
}

fn test_example_1() {
    expect(isMatch("aa", "a") == false);
}

fn test_example_2() {
    expect(isMatch("aa", "a*") == true);
}

fn test_example_3() {
    expect(isMatch("ab", ".*") == true);
}

fn test_example_4() {
    expect(isMatch("aab", "c*a*b") == true);
}

fn test_example_5() {
    expect(isMatch("mississippi", "mis*is*p*.") == false);
}

fn main() {
    test_example_1();
    test_example_2();
    test_example_3();
    test_example_4();
    test_example_5();
}

fn expect(cond: bool) {
    if !cond {
        panic!("expect failed");
    }
}
