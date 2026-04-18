use std::io::{self, Read};
fn main() {
    let mut input = String::new(); io::stdin().read_to_string(&mut input).unwrap();
    if input.is_empty() { return; }
    let lines: Vec<&str> = input.split('\n').map(|s| s.trim_end_matches('\r')).collect();
    let mut idx = 0usize; let t: usize = lines[idx].trim().parse().unwrap_or(0); idx += 1;
    let mut out = Vec::new();
    for _ in 0..t {
        let n: usize = lines.get(idx).unwrap_or(&"0").trim().parse().unwrap_or(0); idx += 1;
        let mut arr: Vec<i32> = Vec::new();
        for _ in 0..n { arr.push(lines.get(idx).unwrap_or(&"0").trim().parse().unwrap_or(0)); idx += 1; }
        let k: usize = lines.get(idx).unwrap_or(&"1").trim().parse().unwrap_or(1); idx += 1;
        let mut i = 0usize;
        while i + k <= arr.len() {
            arr[i..i+k].reverse();
            i += k;
        }
        out.push(format!("[{}]", arr.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(",")));
    }
    print!("{}", out.join("\n"));
}
