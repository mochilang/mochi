fn romanToInt(s: &str) -> i64 {
    let mut values: std::collections::HashMap<String, i64> = std::collections::HashMap::from([
        ("I".to_string(), 1),
        ("V".to_string(), 5),
        ("X".to_string(), 10),
        ("L".to_string(), 50),
        ("C".to_string(), 100),
        ("D".to_string(), 500),
        ("M".to_string(), 1000),
    ]);
    let mut total = 0;
    let mut i = 0;
    let mut n = s.len() as i64;
    while i < n {
        let mut curr = values[({
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
        }) as usize];
        if i + 1 < n {
            let mut next = values[({
                let s = &s;
                let mut idx = i + 1;
                let chars: Vec<char> = s.chars().collect();
                if idx < 0 {
                    idx += chars.len() as i64;
                }
                if idx < 0 || idx >= chars.len() as i64 {
                    panic!("index out of range");
                }
                chars[idx as usize].to_string()
            }) as usize];
            if curr < next {
                total = total + next - curr;
                i = i + 2;
                continue;
            }
        }
        total = total + curr;
        i = i + 1;
    }
    return total;
}

fn test_example_1() {
    expect(romanToInt("III") == 3);
}

fn test_example_2() {
    expect(romanToInt("LVIII") == 58);
}

fn test_example_3() {
    expect(romanToInt("MCMXCIV") == 1994);
}

fn test_subtractive() {
    expect(romanToInt("IV") == 4);
    expect(romanToInt("IX") == 9);
}

fn test_tens() {
    expect(romanToInt("XL") == 40);
    expect(romanToInt("XC") == 90);
}

fn main() {
    test_example_1();
    test_example_2();
    test_example_3();
    test_subtractive();
    test_tens();
}

fn expect(cond: bool) {
    if !cond {
        panic!("expect failed");
    }
}
