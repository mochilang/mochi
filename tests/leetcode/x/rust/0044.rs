use std::io::{self, Read};

fn is_match(s: &str, p: &str) -> bool {
    let sb = s.as_bytes();
    let pb = p.as_bytes();
    let (mut i, mut j, mut star, mut mt) = (0usize, 0usize, None, 0usize);
    while i < sb.len() {
        if j < pb.len() && (pb[j] == b'?' || pb[j] == sb[i]) { i += 1; j += 1; }
        else if j < pb.len() && pb[j] == b'*' { star = Some(j); mt = i; j += 1; }
        else if let Some(sj) = star { j = sj + 1; mt += 1; i = mt; }
        else { return false; }
    }
    while j < pb.len() && pb[j] == b'*' { j += 1; }
    j == pb.len()
}

fn main() {
    let mut input = String::new(); io::stdin().read_to_string(&mut input).unwrap();
    let lines: Vec<String> = input.lines().map(|s| s.to_string()).collect();
    if lines.is_empty() || lines[0].trim().is_empty() { return; }
    let mut idx = 0usize;
    let t: usize = lines[idx].trim().parse().unwrap(); idx += 1;
    let mut out = Vec::with_capacity(t);
    for _ in 0..t {
        let n: usize = lines[idx].trim().parse().unwrap(); idx += 1;
        let s = if n > 0 { let v = lines[idx].clone(); idx += 1; v } else { String::new() };
        let m: usize = lines[idx].trim().parse().unwrap(); idx += 1;
        let p = if m > 0 { let v = lines[idx].clone(); idx += 1; v } else { String::new() };
        out.push(if is_match(&s, &p) { "true".to_string() } else { "false".to_string() });
    }
    print!("{}", out.join("\n"));
}
