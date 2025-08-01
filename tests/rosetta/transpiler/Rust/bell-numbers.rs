// Generated by Mochi transpiler v0.10.42 on 2025-07-27 17:23 +0700
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
fn main() {
        let _start_mem: i64 = _mem();
    let _start: i64 = _now();
    fn bellTriangle(mut n: i64) -> Vec<Vec<i64>> {
    let mut tri: Vec<Vec<i64>> = vec![];
    let mut i: i64 = 0;
    while (i < n) {
        let mut row: Vec<i64> = vec![];
        let mut j: i64 = 0;
        while (j < i) {
            row = { let mut _v = row.clone(); _v.push(0); _v };
            j = (j + 1);
        }
        tri = { let mut _v = tri.clone(); _v.push(row.clone()); _v };
        i = (i + 1);
    }
    tri[1 as usize][0 as usize] = 1;
    i = 2;
    while (i < n) {
        tri[i as usize][0 as usize] = tri[(i - 1) as usize].clone()[(i - 2) as usize];
        let mut j: i64 = 1;
        while (j < i) {
            tri[i as usize][j as usize] = (tri[i as usize].clone()[(j - 1) as usize] + tri[(i - 1) as usize].clone()[(j - 1) as usize]);
            j = (j + 1);
        }
        i = (i + 1);
    }
    return tri
};
    fn mochi_main() {
    let mut bt: Vec<Vec<i64>> = bellTriangle(51);
    println!("{}", "First fifteen and fiftieth Bell numbers:");
    for i in 1..16 {
        println!("{}", format!("{}{}", format!("{}{}", format!("{}{}", "", padStart(i.to_string(), 2, " ")), ": "), bt[i as usize].clone()[0 as usize].to_string()));
    }
    println!("{}", format!("{}{}", "50: ", bt[50 as usize].clone()[0 as usize].to_string()));
    println!("{}", "");
    println!("{}", "The first ten rows of Bell's triangle:");
    for i in 1..11 {
        println!("{:?}", bt[i as usize].clone());
    }
};
    mochi_main();
    let _end: i64 = _now();
    let _end_mem: i64 = _mem();
    let duration_us: i64 = ((_end - _start) / 1000);
    let memory_bytes: i64 = (_end_mem - _start_mem);
    println!("{{\n  \"duration_us\": {},\n  \"memory_bytes\": {},\n  \"name\": \"{}\"\n}}", duration_us, memory_bytes, "main");

}
