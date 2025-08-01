// Generated by Mochi transpiler v0.10.54 on 2025-08-02 11:55 +0700
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
static mut g_PI: f64 = 0.0;
static mut g_nframes: i64 = 0;
static mut g_w: i64 = 0;
static mut g_h: i64 = 0;
static mut g_total: i64 = 0;
static mut g_f: i64 = 0;
fn main() {
    unsafe {
        g_PI = 3.141592653589793;
        g_nframes = 10;
        g_w = 32;
        g_h = 32;
        g_total = 0;
        g_f = 1;
                let _start_mem: i64 = _mem();
        let _start: i64 = _now();
        static mut g_PI: f64 = 0.0;;
        unsafe fn floorf(mut x: f64) -> f64 {
    let mut i: i64 = (x as i64);
    if ((i as f64) > x) {
        i = (i - 1);
    }
    return (i as f64)
};
        unsafe fn frac(mut x: f64) -> f64 {
    return (x - floorf(x))
};
        unsafe fn sinApprox(mut x: f64) -> f64 {
    let mut term: f64 = x;
    let mut sum: f64 = x;
    let mut n: i64 = 1;
    while (n <= 10) {
        let mut denom: f64 = (((2 * n) * ((2 * n) + 1)) as f64);
        term = (((-term * x) * x) / denom);
        sum = (sum + term);
        n = (n + 1);
    }
    return sum
};
        unsafe fn sqrtApprox(mut x: f64) -> f64 {
    if (x <= (0 as f64)) {
        return 0.0
    }
    let mut guess: f64 = x;
    let mut i: i64 = 0;
    while (i < 10) {
        guess = ((guess + (x / guess)) / 2.0);
        i = (i + 1);
    }
    return guess
};
        static mut g_nframes: i64 = 0;;
        static mut g_w: i64 = 0;;
        static mut g_h: i64 = 0;;
        static mut g_total: i64 = 0;;
        static mut g_f: i64 = 0;;
        while (g_f <= g_nframes) {
            let mut y: i64 = 0;
            while (y < g_h) {
                let mut x: i64 = 0;
                while (x < g_w) {
                    let mut fx: f64 = (x as f64);
                    let mut fy: f64 = (y as f64);
                    let mut value: f64 = sinApprox((fx / 16.0));
                    value = (value + sinApprox((fy / 8.0)));
                    value = (value + sinApprox(((fx + fy) / 16.0)));
                    value = (value + sinApprox((sqrtApprox(((fx * fx) + (fy * fy))) / 8.0)));
                    value = (value + 4.0);
                    value = (value / 8.0);
                    let mut rem: f64 = frac((value + ((g_f as f64) / (g_nframes as f64))));
                    let mut ci: i64 = ((((g_nframes as f64) * rem) as i64) + 1);
                    g_total = (g_total + ci);
                    x = (x + 1);
                }
                y = (y + 1);
            }
            g_f = (g_f + 1);
        }
        println!("{}", g_total);
        let _end: i64 = _now();
        let _end_mem: i64 = _mem();
        let duration_us: i64 = ((_end - _start) / 1000);
        let memory_bytes: i64 = (_end_mem - _start_mem);
        println!("{{\n  \"duration_us\": {},\n  \"memory_bytes\": {},\n  \"name\": \"{}\"\n}}", duration_us, memory_bytes, "main");

    }
}
