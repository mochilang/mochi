fn generateParenthesis(n: i64) -> Vec<String> {
    let mut result: Vec<String> = vec![];
    fn backtrack(current: &str, open: i64, close: i64) {
        if current.len() as i64 == n * 2 {
            result = {
                let a = &result;
                let b = &vec![current];
                let mut res = Vec::with_capacity(a.len() + b.len());
                res.extend_from_slice(a);
                res.extend_from_slice(b);
                res
            };
        } else {
            if open < n {
                backtrack(format!("{}{}", current, "("), open + 1, close);
            }
            if close < open {
                backtrack(format!("{}{}", current, ")"), open, close + 1);
            }
        }
    }
    backtrack("", 0, 0);
    return result;
}

fn test_example_1() {
    expect(
        generateParenthesis(3)
            == vec![
                "((()))".to_string(),
                "(()())".to_string(),
                "(())()".to_string(),
                "()(())".to_string(),
                "()()()".to_string(),
            ],
    );
}

fn test_example_2() {
    expect(generateParenthesis(1) == vec!["()".to_string()]);
}

fn test_two_pairs() {
    expect(generateParenthesis(2) == vec!["(())".to_string(), "()()".to_string()]);
}

fn main() {
    test_example_1();
    test_example_2();
    test_two_pairs();
}

fn expect(cond: bool) {
    if !cond {
        panic!("expect failed");
    }
}
