fn letterCombinations(digits: &str) -> Vec<String> {
    if digits.len() as i64 == 0 {
        return vec![];
    }
    let mut mapping = std::collections::HashMap::from([
        (
            "2".to_string(),
            vec!["a".to_string(), "b".to_string(), "c".to_string()],
        ),
        (
            "3".to_string(),
            vec!["d".to_string(), "e".to_string(), "f".to_string()],
        ),
        (
            "4".to_string(),
            vec!["g".to_string(), "h".to_string(), "i".to_string()],
        ),
        (
            "5".to_string(),
            vec!["j".to_string(), "k".to_string(), "l".to_string()],
        ),
        (
            "6".to_string(),
            vec!["m".to_string(), "n".to_string(), "o".to_string()],
        ),
        (
            "7".to_string(),
            vec![
                "p".to_string(),
                "q".to_string(),
                "r".to_string(),
                "s".to_string(),
            ],
        ),
        (
            "8".to_string(),
            vec!["t".to_string(), "u".to_string(), "v".to_string()],
        ),
        (
            "9".to_string(),
            vec![
                "w".to_string(),
                "x".to_string(),
                "y".to_string(),
                "z".to_string(),
            ],
        ),
    ]);
    let mut result = vec!["".to_string()];
    for d_ch in digits.chars() {
        let d = d_ch.to_string();
        if !(mapping.contains_key(&d)) {
            continue;
        }
        let mut letters = mapping[d as usize];
        let mut next = {
            let mut _res = Vec::new();
            for p in result.clone() {
                for ch in letters.clone() {
                    _res.push(p + ch);
                }
            }
            _res
        };
        result = next;
    }
    return result;
}

fn test_example_1() {
    expect(
        letterCombinations("23")
            == vec![
                "ad".to_string(),
                "ae".to_string(),
                "af".to_string(),
                "bd".to_string(),
                "be".to_string(),
                "bf".to_string(),
                "cd".to_string(),
                "ce".to_string(),
                "cf".to_string(),
            ],
    );
}

fn test_example_2() {
    expect(letterCombinations("") == vec![]);
}

fn test_example_3() {
    expect(letterCombinations("2") == vec!["a".to_string(), "b".to_string(), "c".to_string()]);
}

fn test_single_seven() {
    expect(
        letterCombinations("7")
            == vec![
                "p".to_string(),
                "q".to_string(),
                "r".to_string(),
                "s".to_string(),
            ],
    );
}

fn test_mix() {
    expect(
        letterCombinations("79")
            == vec![
                "pw".to_string(),
                "px".to_string(),
                "py".to_string(),
                "pz".to_string(),
                "qw".to_string(),
                "qx".to_string(),
                "qy".to_string(),
                "qz".to_string(),
                "rw".to_string(),
                "rx".to_string(),
                "ry".to_string(),
                "rz".to_string(),
                "sw".to_string(),
                "sx".to_string(),
                "sy".to_string(),
                "sz".to_string(),
            ],
    );
}

fn main() {
    test_example_1();
    test_example_2();
    test_example_3();
    test_single_seven();
    test_mix();
}

fn expect(cond: bool) {
    if !cond {
        panic!("expect failed");
    }
}
