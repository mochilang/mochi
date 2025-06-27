fn convert(s: &str, numRows: i64) -> String {
    if numRows <= 1 || numRows >= s.len() as i64 {
        return s.to_string();
    }
    let mut rows: Vec<String> = vec![];
    let mut i = 0;
    while i < numRows {
        rows = {
            let a = &rows;
            let b = &vec!["".to_string()];
            let mut res = Vec::with_capacity(a.len() + b.len());
            res.extend_from_slice(a);
            res.extend_from_slice(b);
            res
        };
        i = i + 1;
    }
    let mut curr = 0;
    let mut step = 1;
    for ch_ch in s.chars() {
        let ch = ch_ch.to_string();
        rows[curr as usize] = format!("{}{}", rows[curr as usize], ch);
        if curr == 0 {
            step = 1;
        } else if curr == numRows - 1 {
            step = -1;
        }
        curr = curr + step;
    }
    let mut result: String = "".to_string();
    for row in rows {
        result = format!("{}{}", result, row);
    }
    return result.to_string();
}

fn test_example_1() {
    expect(convert("PAYPALISHIRING", 3) == "PAHNAPLSIIGYIR");
}

fn test_example_2() {
    expect(convert("PAYPALISHIRING", 4) == "PINALSIGYAHRPI");
}

fn test_single_row() {
    expect(convert("A", 1) == "A");
}

fn main() {
    test_example_1();
    test_example_2();
    test_single_row();
}

fn expect(cond: bool) {
    if !cond {
        panic!("expect failed");
    }
}
