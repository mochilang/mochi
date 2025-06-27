fn longestCommonPrefix(strs: Vec<String>) -> String {
    if strs.len() as i64 == 0 {
        return "".to_string();
    }
    let mut prefix = strs[(0) as usize];
    for i in 1..strs.len() as i64 {
        let mut j = 0;
        let mut current = strs[i as usize];
        while j < prefix.len() as i64 && j < current.len() as i64 {
            if {
                let s = &prefix;
                let mut idx = j;
                let chars: Vec<char> = s.chars().collect();
                if idx < 0 {
                    idx += chars.len() as i64;
                }
                if idx < 0 || idx >= chars.len() as i64 {
                    panic!("index out of range");
                }
                chars[idx as usize].to_string()
            } != {
                let s = &current;
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
        prefix = _slice_string(prefix, 0, j);
        if prefix == "" {
            break;
        }
    }
    return prefix.to_string();
}

fn test_example_1() {
    expect(
        longestCommonPrefix(vec![
            "flower".to_string(),
            "flow".to_string(),
            "flight".to_string(),
        ]) == "fl",
    );
}

fn test_example_2() {
    expect(
        longestCommonPrefix(vec![
            "dog".to_string(),
            "racecar".to_string(),
            "car".to_string(),
        ]) == "",
    );
}

fn test_single_string() {
    expect(longestCommonPrefix(vec!["single".to_string()]) == "single");
}

fn test_no_common_prefix() {
    expect(longestCommonPrefix(vec!["a".to_string(), "b".to_string(), "c".to_string()]) == "");
}

fn main() {
    test_example_1();
    test_example_2();
    test_single_string();
    test_no_common_prefix();
}

fn _slice_string(s: &str, start: i64, end: i64) -> String {
    let mut sidx = start;
    let mut eidx = end;
    let chars: Vec<char> = s.chars().collect();
    let n = chars.len() as i64;
    if sidx < 0 {
        sidx += n;
    }
    if eidx < 0 {
        eidx += n;
    }
    if sidx < 0 {
        sidx = 0;
    }
    if eidx > n {
        eidx = n;
    }
    if eidx < sidx {
        eidx = sidx;
    }
    chars[sidx as usize..eidx as usize].iter().collect()
}
fn expect(cond: bool) {
    if !cond {
        panic!("expect failed");
    }
}
