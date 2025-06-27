fn isValid(s: &str) -> bool {
    let mut stack: Vec<String> = vec![];
    let mut n = s.len() as i64;
    for i in 0..n {
        let mut c = {
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
        };
        if c == "(" {
            stack = {
                let a = &stack;
                let b = &vec![")".to_string()];
                let mut res = Vec::with_capacity(a.len() + b.len());
                res.extend_from_slice(a);
                res.extend_from_slice(b);
                res
            };
        } else if c == "[" {
            stack = {
                let a = &stack;
                let b = &vec!["]".to_string()];
                let mut res = Vec::with_capacity(a.len() + b.len());
                res.extend_from_slice(a);
                res.extend_from_slice(b);
                res
            };
        } else if c == "{" {
            stack = {
                let a = &stack;
                let b = &vec!["}".to_string()];
                let mut res = Vec::with_capacity(a.len() + b.len());
                res.extend_from_slice(a);
                res.extend_from_slice(b);
                res
            };
        } else {
            if stack.len() as i64 == 0 {
                return false;
            }
            let mut top = stack[(stack.len() as i64 - 1) as usize];
            if top != c {
                return false;
            }
            stack = stack[(0) as usize..(stack.len() as i64 - 1) as usize].to_vec();
        }
    }
    return stack.len() as i64 == 0;
}

fn test_example_1() {
    expect(isValid("()") == true);
}

fn test_example_2() {
    expect(isValid("()[]{}") == true);
}

fn test_example_3() {
    expect(isValid("(]") == false);
}

fn test_example_4() {
    expect(isValid("([)]") == false);
}

fn test_example_5() {
    expect(isValid("{[]}") == true);
}

fn test_empty_string() {
    expect(isValid("") == true);
}

fn test_single_closing() {
    expect(isValid("]") == false);
}

fn test_unmatched_open() {
    expect(isValid("((") == false);
}

fn main() {
    test_example_1();
    test_example_2();
    test_example_3();
    test_example_4();
    test_example_5();
    test_empty_string();
    test_single_closing();
    test_unmatched_open();
}

fn expect(cond: bool) {
    if !cond {
        panic!("expect failed");
    }
}
