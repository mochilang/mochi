fn convert(s: &str, num_rows: i32) -> String {
    if num_rows <= 1 || num_rows as usize >= s.len() {
        return s.to_string();
    }
    let mut rows: Vec<String> = Vec::new();
    let mut i = 0;
    while i < num_rows {
        rows = _concat(&rows, &vec![String::new()]);
        i += 1;
    }
    let mut curr = 0;
    let mut step = 1;
    for ch_ch in s.chars() {
        let ch = ch_ch.to_string();
        rows[curr as usize] = format!("{}{}", rows[curr as usize], ch);
        if curr == 0 {
            step = 1;
        } else if curr == num_rows - 1 {
            step = -1;
        }
        curr += step;
    }
    let mut result = String::new();
    for row in rows {
        result = format!("{}{}", result, row);
    }
    result
}

fn main() {}

fn _concat<T: Clone>(a: &[T], b: &[T]) -> Vec<T> {
    let mut res = Vec::with_capacity(a.len() + b.len());
    res.extend_from_slice(a);
    res.extend_from_slice(b);
    res
}
