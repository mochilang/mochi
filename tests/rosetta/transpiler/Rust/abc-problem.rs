// Generated by Mochi transpiler v0.10.54 on 2025-08-02 14:06 +0700
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
    fn fields(mut s: String) -> Vec<String> {
    let mut res: Vec<String> = vec![];
    let mut cur: String = String::from("").clone();
    let mut i: i64 = 0;
    while (i < (s.len() as i64)) {
        let mut c: String = s[i as usize..(i + 1) as usize].to_string().clone();
        if (c.as_str() == " ") {
            if ((cur.len() as i64) > 0) {
                res = { let mut _v = res.clone(); _v.push(cur.clone()); _v };
                cur = String::from("");
            }
        } else {
            cur = format!("{}{}", cur, c);
        }
        i = (i + 1);
    }
    if ((cur.len() as i64) > 0) {
        res = { let mut _v = res.clone(); _v.push(cur.clone()); _v };
    }
    return res
};
    fn canSpell(mut word: String, blks: &mut Vec<String>) -> bool {
    if ((word.len() as i64) == 0) {
        return true
    }
    let mut c: String = (word[0 as usize..1 as usize].to_string().to_lowercase()).clone();
    let mut i: i64 = 0;
    while (i < (blks.len() as i64)) {
        let mut b: String = blks[i as usize].clone().clone();
        if ((c.as_str() == (b[0 as usize..1 as usize].to_string().to_lowercase()).as_str()) || (c.as_str() == (b[1 as usize..2 as usize].to_string().to_lowercase()).as_str())) {
            let mut rest: Vec<String> = vec![];
            let mut j: i64 = 0;
            while (j < (blks.len() as i64)) {
                if (j != i) {
                    rest = { let mut _v = rest.clone(); _v.push(blks[j as usize].clone()); _v };
                }
                j = (j + 1);
            }
            if canSpell(word[1 as usize..].to_string(), &mut rest) {
                return true
            }
        }
        i = (i + 1);
    }
    return false
};
    fn newSpeller(blocks: &str) -> impl FnMut(String) -> bool {
    let mut bl: Vec<String> = fields(blocks.to_string());
    return move |w: String| -> bool { canSpell(w.to_string(), &mut bl) }
};
    fn mochi_main() {
    let mut sp = newSpeller(&"BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM");
    for word in vec![String::from("A"), String::from("BARK"), String::from("BOOK"), String::from("TREAT"), String::from("COMMON"), String::from("SQUAD"), String::from("CONFUSE")] {
        println!("{}", format!("{}{}", format!("{}{}", word, " "), sp(word).to_string()));
    }
};
    mochi_main();
    let _end: i64 = _now();
    let _end_mem: i64 = _mem();
    let duration_us: i64 = ((_end - _start) / 1000);
    let memory_bytes: i64 = (_end_mem - _start_mem);
    println!("{{\n  \"duration_us\": {},\n  \"memory_bytes\": {},\n  \"name\": \"{}\"\n}}", duration_us, memory_bytes, "main");

}
