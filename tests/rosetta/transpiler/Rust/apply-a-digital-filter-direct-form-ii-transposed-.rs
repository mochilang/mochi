// Generated by Mochi transpiler v0.10.55 on 2025-08-02 20:52 +0700
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
static mut g_b: Vec<f64> = Vec::new();
static mut g_k: i64 = 0;
fn main() {
    unsafe {
        g_b = vec![0.16666667, 0.5, 0.5, 0.16666667];
        g_k = 0;
                let _start_mem: i64 = _mem();
        let _start: i64 = _now();
        unsafe fn applyFilter(mut input: Vec<f64>, mut a: Vec<f64>, mut b: Vec<f64>) -> Vec<f64> {
    let mut out: Vec<f64> = vec![];
    let mut scale: f64 = (1.0 / a[0 as usize]);
    let mut i: i64 = 0;
    while (i < (input.len() as i64)) {
        let mut tmp: f64 = 0.0;
        let mut j: i64 = 0;
        while ((j <= i) && (j < (b.len() as i64))) {
            tmp = (tmp + (b[j as usize] * input[(i - j) as usize]));
            j = (j + 1);
        }
        j = 0;
        while ((j < i) && ((j + 1) < (a.len() as i64))) {
            tmp = (tmp - (a[(j + 1) as usize] * out[((i - j) - 1) as usize]));
            j = (j + 1);
        }
        out = { let mut _v = out.clone(); _v.push((tmp * scale)); _v };
        i = (i + 1);
    }
    return out
};
        let mut a: Vec<f64> = vec![1.0, -0.00000000000000027756, 0.33333333, -0.0000000000000000185];
        static mut g_b: Vec<f64> = Vec::new();;
        let mut sig: Vec<f64> = vec![-0.917843918645, 0.141984778794, 1.20536903482, 0.190286794412, -0.662370894973, -1.00700480494, -0.404707073677, 0.800482325044, 0.743500089861, 1.01090520172, 0.741527555207, 0.277841675195, 0.400833448236, -0.2085993586, -0.172842103641, -0.134316096293, 0.0259303398477, 0.490105989562, 0.549391221511, 0.9047198589];
        let mut res: Vec<f64> = applyFilter(sig.clone(), a.clone(), g_b.clone().clone());
        static mut g_k: i64 = 0;;
        while (g_k < (res.len() as i64)) {
            println!("{}", format!("{:?}", res[g_k as usize]));
            g_k = (g_k + 1);
        }
        let _end: i64 = _now();
        let _end_mem: i64 = _mem();
        let duration_us: i64 = ((_end - _start) / 1000);
        let memory_bytes: i64 = (_end_mem - _start_mem);
        println!("{{\n  \"duration_us\": {},\n  \"memory_bytes\": {},\n  \"name\": \"{}\"\n}}", duration_us, memory_bytes, "main");

    }
}
