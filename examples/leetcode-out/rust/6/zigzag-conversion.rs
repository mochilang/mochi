fn convert(s: &str, numRows: i32) -> String {
    if numRows <= 1 || numRows >= s.len() as i32 {
        return s.to_string();
    }
    let mut rows = vec![];
    let mut i = 0;
    while i < numRows {
        rows = _concat(&rows, &vec![""]);
        i = i + 1;
    }
    let mut curr = 0;
    let mut step = 1;
    for ch in s.chars() {
        rows[(curr) as usize] = rows[(curr) as usize] + ch;
        if curr == 0 {
            step = 1;
        } else 
        if curr == numRows - 1 {
            step = -1;
        }
        curr = curr + step;
    }
    let mut result = "";
    for row in rows {
        result = result + row;
    }
    return result;
}

fn main() {
}

fn _concat<T: Clone>(a: &[T], b: &[T]) -> Vec<T> {
    let mut res = Vec::with_capacity(a.len() + b.len());
    res.extend_from_slice(a);
    res.extend_from_slice(b);
    res
}
