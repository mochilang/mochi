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
static mut g_FASTA: String = String::new();
fn main() {
    unsafe {
        g_FASTA = format!("{}{}", format!("{}{}", format!("{}{}", format!("{}{}", format!("{}{}", ">Rosetta_Example_1\n", "THERECANBENOSPACE\n"), ">Rosetta_Example_2\n"), "THERECANBESEVERAL\n"), "LINESBUTTHEYALLMUST\n"), "BECONCATENATED").clone();
                let _start_mem: i64 = _mem();
        let _start: i64 = _now();
        static mut g_FASTA: String = String::new();;
        unsafe fn splitLines(s: &str) -> Vec<String> {
    let mut lines: Vec<String> = vec![];
    let mut start: i64 = 0;
    let mut i: i64 = 0;
    while (i < (s.len() as i64)) {
        if ({ let tmp = &s; tmp.chars().skip(i as usize).take(((i + 1) - i) as usize).collect::<String>() }.as_str() == "\n") {
            lines = { let mut _v = lines.clone(); _v.push({ let tmp = &s; tmp.chars().skip(start as usize).take((i - start) as usize).collect::<String>() }.to_string()); _v };
            i = (i + 1);
            start = i;
        } else {
            i = (i + 1);
        }
    }
    lines = { let mut _v = lines.clone(); _v.push({ let tmp = &s; tmp.chars().skip(start as usize).take(((s.len() as i64) - start) as usize).collect::<String>() }.to_string()); _v };
    return lines
};
        unsafe fn parseFasta(text: &str) -> Vec<String> {
    let mut key = String::from("");
    let mut val = String::from("");
    let mut out: Vec<String> = vec![];
    for line in splitLines(text) {
        if (line.as_str() == "") {
            continue
        }
        if ({ let tmp = &line; tmp.chars().skip(0 as usize).take((1 - 0) as usize).collect::<String>() }.as_str() == ">") {
            if (key.as_str() != "") {
                out = { let mut _v = out.clone(); _v.push(format!("{}{}", format!("{}{}", key, ": "), val).to_string()); _v };
            }
            let mut hdr: String = { let tmp = &line; tmp.chars().skip(1 as usize).take(((line.len() as i64) - 1) as usize).collect::<String>() }.clone();
            let mut idx: i64 = 0;
            while ((idx < (hdr.len() as i64)) && ({ let tmp = &hdr; tmp.chars().skip(idx as usize).take(((idx + 1) - idx) as usize).collect::<String>() }.as_str() != " ")) {
                idx = (idx + 1);
            }
            key = { let tmp = &hdr; tmp.chars().skip(0 as usize).take((idx - 0) as usize).collect::<String>() };
            val = String::from("");
        } else {
            if (key.as_str() == "") {
                println!("{}", "missing header");
                return vec![]
            }
            val = format!("{}{}", val, line);
        }
    }
    if (key.as_str() != "") {
        out = { let mut _v = out.clone(); _v.push(format!("{}{}", format!("{}{}", key, ": "), val).to_string()); _v };
    }
    return out
};
        unsafe fn mochi_main() {
    let mut res: Vec<String> = parseFasta(&g_FASTA);
    for line in res.clone() {
        println!("{}", line);
    }
};
        mochi_main();
        let _end: i64 = _now();
        let _end_mem: i64 = _mem();
        let duration_us: i64 = ((_end - _start) / 1000);
        let memory_bytes: i64 = (_end_mem - _start_mem);
        println!("{{\n  \"duration_us\": {},\n  \"memory_bytes\": {},\n  \"name\": \"{}\"\n}}", duration_us, memory_bytes, "main");

    }
}
