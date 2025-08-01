// Generated by Mochi transpiler v0.10.42 on 2025-07-27 17:23 +0700
use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, AtomicI64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};
static NOW_SEEDED: AtomicBool = AtomicBool::new(false);
static NOW_SEED: AtomicI64 = AtomicI64::new(0);
fn _now() -> i64 {
    if !NOW_SEEDED.load(Ordering::SeqCst) {
        if let Ok(s) = std::env::var("MOCHI_NOW_SEED") {
            if let Ok(v) = s.parse::<i64>() {
                NOW_SEED.store(v, Ordering::SeqCst);
                NOW_SEEDED.store(true, Ordering::SeqCst);
            }
        }
    }
    if NOW_SEEDED.load(Ordering::SeqCst) {
        let seed = (NOW_SEED.load(Ordering::SeqCst)*1664525 + 1013904223) % 2147483647;
        NOW_SEED.store(seed, Ordering::SeqCst);
        seed
    } else {
        SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_nanos() as i64
    }
}
fn _mem() -> i64 {
    if let Ok(mut f) = std::fs::File::open("/proc/self/statm") {
        let mut s = String::new();
        use std::io::Read;
        if f.read_to_string(&mut s).is_ok() {
            if let Some(p) = s.split_whitespace().next() {
                if let Ok(v) = p.parse::<i64>() {
                    return v * 4096;
                }
            }
        }
    }
    0
}
#[derive(Debug, Clone, Default)]
struct Map {
    s: i64,
    a: i64,
    b: i64,
}
impl std::fmt::Display for Map {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        write!(f, "\"s\": {}", self.s)?;
        write!(f, ", ")?;
        write!(f, "\"a\": {}", self.a)?;
        write!(f, ", ")?;
        write!(f, "\"b\": {}", self.b)?;
        write!(f, "}}")
    }
}

fn main() {
        let _start_mem: i64 = _mem();
    let _start: i64 = _now();
    fn push(mut h: Vec<HashMap<String, i64>>, it: &HashMap<String, i64>) -> Vec<HashMap<String, i64>> {
    h = { let mut _v = h.clone(); _v.push(it.clone()); _v };
    let mut i = ((h.len() as i64) - 1);
    while ((i > 0) && (h[(i - 1) as usize].clone()["s".as_str()] > h[i as usize].clone()["s".as_str()])) {
        let mut tmp: HashMap<String, i64> = h[(i - 1) as usize].clone();
        h[(i - 1) as usize] = h[i as usize].clone();
        h[i as usize] = tmp;
        i = (i - 1);
    }
    return h
};
    fn step(mut h: Vec<HashMap<String, i64>>, mut nv: i64, mut dir: Vec<i64>) -> HashMap<&str, &Vec<i64>> {
    while (((h.len() as i64) == 0) || ((nv * nv) <= h[0 as usize].clone()["s".as_str()])) {
        h = push(h.clone(), &Map {s: (nv * nv), a: nv, b: 0});
        nv = (nv + 1);
    }
    let s: i64 = h[0 as usize].clone()["s".as_str()];
    let mut v: Vec<Vec<i64>> = vec![];
    while (((h.len() as i64) > 0) && (h[0 as usize].clone()["s".as_str()] == s)) {
        let mut it: HashMap<String, i64> = h[0 as usize].clone();
        h = h[1 as usize..].to_vec();
        v = { let mut _v = v.clone(); _v.push(vec![it["a".as_str()], it["b".as_str()]]); _v };
        if (it["a".as_str()] > it["b".as_str()]) {
            h = push(h.clone(), &HashMap::from([(String::from("s"), ((it["a".as_str()] * it["a".as_str()]) + ((it["b".as_str()] + 1) * (it["b".as_str()] + 1)))), (String::from("a"), it["a".as_str()]), (String::from("b"), (it["b".as_str()] + 1))]));
        }
    }
    let mut list: Vec<Vec<i64>> = vec![];
    for p in &v {
        list = { let mut _v = list.clone(); _v.push(p); _v };
    }
    let mut temp: Vec<Vec<i64>> = list;
    for p in &temp {
        if (p[0] != p[1]) {
            list = { let mut _v = list.clone(); _v.push(vec![p[1], p[0]]); _v };
        }
    }
    temp = list.clone();
    for p in &temp {
        if (p[1] != 0) {
            list = { let mut _v = list.clone(); _v.push(vec![p[0], -p[1]]); _v };
        }
    }
    temp = list.clone();
    for p in &temp {
        if (p[0] != 0) {
            list = { let mut _v = list.clone(); _v.push(vec![-p[0], p[1]]); _v };
        }
    }
    let mut bestDot: i64 = -999999999;
    let mut best: Vec<i64> = dir;
    for p in &list {
        let cross: i64 = ((p[0] * dir[1 as usize]) - (p[1] * dir[0 as usize]));
        if (cross >= 0) {
            let dot: i64 = ((p[0] * dir[0 as usize]) + (p[1] * dir[1 as usize]));
            if (dot > bestDot) {
                bestDot = dot;
                best = p;
            }
        }
    }
    return HashMap::from([(String::from("d"), best), (String::from("heap"), h), (String::from("n"), nv)])
};
    fn positions(mut n: i64) -> Vec<Vec<i64>> {
    let mut pos: Vec<Vec<i64>> = vec![];
    let mut x: i64 = 0;
    let mut y: i64 = 0;
    let mut dir: Vec<i64> = vec![0, 1];
    let mut heap: Vec<HashMap<String, i64>> = vec![];
    let mut nv: i64 = 1;
    let mut i: i64 = 0;
    while (i < n) {
        pos = { let mut _v = pos.clone(); _v.push(vec![x, y]); _v };
        let mut st: HashMap<&str, &Vec<i64>> = step(heap.clone(), nv, dir.clone());
        dir = st["d".as_str()].clone();
        heap = st["heap".as_str()].clone();
        nv = (st["n".as_str()].clone() as i64);
        x = (x + dir[0]);
        y = (y + dir[1]);
        i = (i + 1);
    }
    return pos
};
    fn pad(mut s: String, mut w: i64) -> String {
    let mut r: String = s.clone();
    while ((r.len() as i64) < w) {
        r = format!("{}{}", r, " ");
    }
    return r.clone()
};
    fn mochi_main() {
    let mut pts: Vec<Vec<i64>> = positions(40);
    println!("{}", "The first 40 Babylonian spiral points are:");
    let mut line = String::from("");
    let mut i: i64 = 0;
    while (i < (pts.len() as i64)) {
        let mut p: Vec<i64> = pts[i as usize].clone();
        let s: String = pad(format!("{}{}", format!("{}{}", format!("{}{}", format!("{}{}", "(", p[0 as usize].to_string()), ", "), p[1 as usize].to_string()), ")"), 10).clone();
        line = format!("{}{}", line, s);
        if (((i + 1) % 10) == 0) {
            println!("{}", line);
            line = String::from("");
        }
        i = (i + 1);
    }
};
    mochi_main();
    let _end: i64 = _now();
    let _end_mem: i64 = _mem();
    let duration_us: i64 = ((_end - _start) / 1000);
    let memory_bytes: i64 = (_end_mem - _start_mem);
    println!("{{\n  \"duration_us\": {},\n  \"memory_bytes\": {},\n  \"name\": \"{}\"\n}}", duration_us, memory_bytes, "main");

}
