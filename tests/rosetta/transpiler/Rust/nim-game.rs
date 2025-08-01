// Generated by Mochi transpiler v0.10.54 on 2025-08-02 11:55 +0700
use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, AtomicI64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};
use std::io::{self, Read};
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
fn input() -> String {
    let mut s = String::new();
    std::io::stdin().read_line(&mut s).unwrap();
    s.trim_end().to_string()
}
fn main() {
        let _start_mem: i64 = _mem();
    let _start: i64 = _now();
    fn parseIntStr(mut str: String) -> i64 {
    let mut i: i64 = 0;
    let mut neg: bool = false;
    if (((str.len() as i64) > 0) && (str[0 as usize..1 as usize].to_string().as_str() == "-")) {
        neg = true;
        i = 1;
    }
    let mut n: i64 = 0;
    let mut digits = HashMap::from([(String::from("0"), 0), (String::from("1"), 1), (String::from("2"), 2), (String::from("3"), 3), (String::from("4"), 4), (String::from("5"), 5), (String::from("6"), 6), (String::from("7"), 7), (String::from("8"), 8), (String::from("9"), 9)]);
    while (i < (str.len() as i64)) {
        n = ((n * 10) + digits[str[i as usize..(i + 1) as usize].to_string().as_str()]);
        i = (i + 1);
    }
    if neg {
        n = -n;
    }
    return n
};
    fn showTokens(mut tokens: i64) {
    println!("{}", format!("{}{}", "Tokens remaining ", tokens.to_string()));
};
    fn mochi_main() {
    let mut tokens: i64 = 12;
    let mut done: bool = false;
    while !done {
        showTokens(tokens);
        println!("{}", "");
        println!("{}", "How many tokens 1, 2 or 3?");
        let mut line: String = input().clone();
        let mut t: i64 = 0;
        if ((line.len() as i64) > 0) {
            t = parseIntStr(line);
        }
        if ((t < 1) || (t > 3)) {
            println!("{}", "\nMust be a number between 1 and 3, try again.\n");
        } else {
            let mut ct: i64 = (4 - t);
            let mut s: String = String::from("s").clone();
            if (ct == 1) {
                s = String::from("");
            }
            println!("{}", format!("{}{}", format!("{}{}", format!("{}{}", format!("{}{}", "  Computer takes ", ct.to_string()), " token"), s), "\n\n"));
            tokens = (tokens - 4);
        }
        if (tokens == 0) {
            showTokens(0);
            println!("{}", "  Computer wins!");
            done = true;
        }
    }
};
    mochi_main();
    let _end: i64 = _now();
    let _end_mem: i64 = _mem();
    let duration_us: i64 = ((_end - _start) / 1000);
    let memory_bytes: i64 = (_end_mem - _start_mem);
    println!("{{\n  \"duration_us\": {},\n  \"memory_bytes\": {},\n  \"name\": \"{}\"\n}}", duration_us, memory_bytes, "main");

}
