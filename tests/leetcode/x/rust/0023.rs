use std::io::{self, Read};
fn main() {
    let mut input = String::new(); io::stdin().read_to_string(&mut input).unwrap();
    if input.is_empty() { return; }
    let lines: Vec<&str> = input.split('\n').map(|s| s.trim_end_matches('\r')).collect();
    let mut idx = 0usize; let t: usize = lines[idx].trim().parse().unwrap_or(0); idx += 1;
    let mut out = Vec::new();
    for _ in 0..t {
        let k: usize = lines.get(idx).unwrap_or(&"0").trim().parse().unwrap_or(0); idx += 1;
        let mut vals: Vec<i32> = Vec::new();
        for _ in 0..k {
            let n: usize = lines.get(idx).unwrap_or(&"0").trim().parse().unwrap_or(0); idx += 1;
            for _ in 0..n { vals.push(lines.get(idx).unwrap_or(&"0").trim().parse().unwrap_or(0)); idx += 1; }
        }
        vals.sort();
        out.push(format!("[{}]", vals.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(",")));
    }
    print!("{}", out.join("\n"));
}
