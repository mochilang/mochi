// Generated by Mochi transpiler v0.10.42 on 2025-07-28 10:12 +0700
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
struct DivResult {
    q: i64,
    r: i64,
}
impl std::fmt::Display for DivResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        write!(f, "\"q\": {}", self.q)?;
        write!(f, ", ")?;
        write!(f, "\"r\": {}", self.r)?;
        write!(f, "}}")
    }
}

fn main() {
        let _start_mem: i64 = _mem();
    let _start: i64 = _now();
    fn egyptianDivide(mut dividend: i64, mut divisor: i64) -> DivResult {
    if ((dividend < 0) || (divisor <= 0)) {
        panic("Invalid argument(s)");
    }
    if (dividend < divisor) {
        return DivResult {q: 0, r: dividend}
    }
    let mut powers: Vec<i64> = vec![1];
    let mut doublings: Vec<i64> = vec![divisor];
    let mut doubling: i64 = (divisor * 2);
    while (doubling <= dividend) {
        powers = { let mut _v = powers.clone(); _v.push((powers[((powers.len() as i64) - 1) as usize] * 2)); _v };
        doublings = { let mut _v = doublings.clone(); _v.push(doubling); _v };
        doubling = (doubling * 2);
    }
    let mut ans: i64 = 0;
    let mut accum: i64 = 0;
    let mut i = ((doublings.len() as i64) - 1);
    while (i >= 0) {
        if ((accum + doublings[i as usize]) <= dividend) {
            accum = (accum + doublings[i as usize]);
            ans = (ans + powers[i as usize]);
            if (accum == dividend) {
                break
            }
        }
        i = (i - 1);
    }
    return DivResult {q: ans, r: (dividend - accum)}
};
    fn mochi_main() {
    let mut dividend: i64 = 580;
    let mut divisor: i64 = 34;
    let mut res: DivResult = egyptianDivide(dividend, divisor);
    println!("{}", format!("{}{}", format!("{}{}", format!("{}{}", format!("{}{}", format!("{}{}", format!("{}{}", dividend.to_string(), " divided by "), divisor.to_string()), " is "), res.q.to_string()), " with remainder "), res.r.to_string()));
};
    mochi_main();
    let _end: i64 = _now();
    let _end_mem: i64 = _mem();
    let duration_us: i64 = ((_end - _start) / 1000);
    let memory_bytes: i64 = (_end_mem - _start_mem);
    println!("{{\n  \"duration_us\": {},\n  \"memory_bytes\": {},\n  \"name\": \"{}\"\n}}", duration_us, memory_bytes, "main");

}
