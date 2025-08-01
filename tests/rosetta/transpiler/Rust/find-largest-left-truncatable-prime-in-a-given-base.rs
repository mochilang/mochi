// Generated by Mochi transpiler v0.10.50 on 2025-07-30 21:05 +0700
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
    fn isPrime(mut n: i64) -> bool {
    if (n < 2) {
        return false
    }
    let mut i: i64 = 2;
    while ((i * i) <= n) {
        if ((n % i) == 0) {
            return false
        }
        i = (i + 1);
    }
    return true
};
    fn search(mut base: i64, mut prefix: i64, mut depth: i64, mut limit: i64, mut best: i64) -> i64 {
    let mut b: i64 = best;
    let mut d: i64 = 1;
    while (d < base) {
        let mut val: i64 = ((prefix * base) + d);
        if isPrime(val) {
            if (val > b) {
                b = val;
            }
            if ((depth + 1) < limit) {
                b = search(base, val, (depth + 1), limit, b);
            }
        }
        d = (d + 1);
    }
    return b
};
    fn largest(mut base: i64) -> i64 {
    return search(base, 0, 0, 6, 0)
};
    fn mochi_main() {
    let mut b: i64 = 3;
    while (b <= 17) {
        println!("{}", format!("{}{}", format!("{}{}", b.to_string(), ": "), largest(b).to_string()));
        b = (b + 1);
    }
};
    mochi_main();
    let _end: i64 = _now();
    let _end_mem: i64 = _mem();
    let duration_us: i64 = ((_end - _start) / 1000);
    let memory_bytes: i64 = (_end_mem - _start_mem);
    println!("{{\n  \"duration_us\": {},\n  \"memory_bytes\": {},\n  \"name\": \"{}\"\n}}", duration_us, memory_bytes, "main");

}
