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
static mut g_seed: i64 = 0;
fn main() {
    unsafe {
        g_seed = 1;
                let _start_mem: i64 = _mem();
        let _start: i64 = _now();
        static mut g_seed: i64 = 0;;
        unsafe fn prng(mut max: i64) -> i64 {
    g_seed = (((g_seed * 1103515245) + 12345) % 2147483648);
    return (g_seed % max)
};
        unsafe fn gen(mut n: i64) -> String {
    let mut arr: Vec<String> = vec![];
    let mut i: i64 = 0;
    while (i < n) {
        arr = { let mut _v = arr.clone(); _v.push("[".to_string()); _v };
        arr = { let mut _v = arr.clone(); _v.push("]".to_string()); _v };
        i = (i + 1);
    }
    let mut j = ((arr.len() as i64) - 1);
    while (j > 0) {
        let k: i64 = prng((j + 1));
        let tmp: String = arr[j as usize].clone().clone();
        arr[j as usize] = arr[k as usize].clone();
        arr[k as usize] = tmp;
        j = (j - 1);
    }
    let mut out = String::from("");
    for ch in arr.clone() {
        out = format!("{}{}", out, ch);
    }
    return out.clone()
};
        unsafe fn testBalanced(mut s: String) {
    let mut open: i64 = 0;
    let mut i: i64 = 0;
    while (i < (s.len() as i64)) {
        let c: String = s[i as usize..(i + 1) as usize].to_string().clone();
        if (c.as_str() == "[") {
            open = (open + 1);
        } else if (c.as_str() == "]") {
            if (open == 0) {
                println!("{}", format!("{}{}", s, ": not ok"));
                return
            }
            open = (open - 1);
        } else {
            println!("{}", format!("{}{}", s, ": not ok"));
            return
        }
        i = (i + 1);
    }
    if (open == 0) {
        println!("{}", format!("{}{}", s, ": ok"));
    } else {
        println!("{}", format!("{}{}", s, ": not ok"));
    }
};
        unsafe fn mochi_main() {
    let mut i: i64 = 0;
    while (i < 10) {
        testBalanced(gen(i));
        i = (i + 1);
    }
    testBalanced(String::from("()"));
};
        mochi_main();
        let _end: i64 = _now();
        let _end_mem: i64 = _mem();
        let duration_us: i64 = ((_end - _start) / 1000);
        let memory_bytes: i64 = (_end_mem - _start_mem);
        println!("{{\n  \"duration_us\": {},\n  \"memory_bytes\": {},\n  \"name\": \"{}\"\n}}", duration_us, memory_bytes, "main");

    }
}
