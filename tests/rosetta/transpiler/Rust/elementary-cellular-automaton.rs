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
fn main() {
        let _start_mem: i64 = _mem();
    let _start: i64 = _now();
    fn bitAt(mut x: i64, mut idx: i64) -> i64 {
    let mut v: i64 = x;
    let mut i: i64 = 0;
    while (i < idx) {
        v = (v / 2);
        i = (i + 1);
    }
    return (v % 2)
};
    fn outputState(state: &str) {
    let mut line = String::from("");
    let mut i: i64 = 0;
    while (i < (state.len() as i64)) {
        if (state[i as usize..(i + 1) as usize].to_string().as_str() == "1") {
            line = format!("{}{}", line, "#");
        } else {
            line = format!("{}{}", line, " ");
        }
        i = (i + 1);
    }
    println!("{}", line);
};
    fn step(state: &str, mut r: i64) -> String {
    let mut cells = (state.len() as i64);
    let mut out = String::from("");
    let mut i: i64 = 0;
    while (i < cells) {
        let mut l: String = state[(((i - 1) + cells) % cells) as usize..((((i - 1) + cells) % cells) + 1) as usize].to_string().clone();
        let mut c: String = state[i as usize..(i + 1) as usize].to_string().clone();
        let mut rt: String = state[((i + 1) % cells) as usize..(((i + 1) % cells) + 1) as usize].to_string().clone();
        let mut idx: i64 = 0;
        if (l.as_str() == "1") {
            idx = (idx + 4);
        }
        if (c.as_str() == "1") {
            idx = (idx + 2);
        }
        if (rt.as_str() == "1") {
            idx = (idx + 1);
        }
        if (bitAt(r, idx) == 1) {
            out = format!("{}{}", out, "1");
        } else {
            out = format!("{}{}", out, "0");
        }
        i = (i + 1);
    }
    return out.clone()
};
    fn elem(mut r: i64, mut cells: i64, mut generations: i64, state: &str) {
    outputState(state);
    let mut g: i64 = 0;
    let mut s: &str = state;
    while (g < generations) {
        s = step(&s, r);
        outputState(&s);
        g = (g + 1);
    }
};
    fn randInit(mut cells: i64, mut seed: i64) -> String {
    let mut s = String::from("");
    let mut val: i64 = seed;
    let mut i: i64 = 0;
    while (i < cells) {
        val = (((val * 1664525) + 1013904223) % 2147483647);
        if ((val % 2) == 0) {
            s = format!("{}{}", s, "0");
        } else {
            s = format!("{}{}", s, "1");
        }
        i = (i + 1);
    }
    return s.clone()
};
    fn singleInit(mut cells: i64) -> String {
    let mut s = String::from("");
    let mut i: i64 = 0;
    while (i < cells) {
        if (i == (cells / 2)) {
            s = format!("{}{}", s, "1");
        } else {
            s = format!("{}{}", s, "0");
        }
        i = (i + 1);
    }
    return s.clone()
};
    fn mochi_main() {
    let mut cells: i64 = 20;
    let mut generations: i64 = 9;
    println!("{}", "Single 1, rule 90:");
    let mut state: String = singleInit(cells).clone();
    elem(90, cells, generations, &state);
    println!("{}", "Random intial state, rule 30:");
    state = randInit(cells, 3);
    elem(30, cells, generations, &state);
};
    mochi_main();
    let _end: i64 = _now();
    let _end_mem: i64 = _mem();
    let duration_us: i64 = ((_end - _start) / 1000);
    let memory_bytes: i64 = (_end_mem - _start_mem);
    println!("{{\n  \"duration_us\": {},\n  \"memory_bytes\": {},\n  \"name\": \"{}\"\n}}", duration_us, memory_bytes, "main");

}
