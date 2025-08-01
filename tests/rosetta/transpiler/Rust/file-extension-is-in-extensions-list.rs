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
#[derive(Debug, Clone, Default)]
struct Ret {
    f0: bool,
    f1: String,
}
impl std::fmt::Display for Ret {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        write!(f, "\"f0\": {}", self.f0)?;
        write!(f, ", ")?;
        write!(f, "\"f1\": \"{}\"", self.f1)?;
        write!(f, "}}")
    }
}

static mut g_extensions: Vec<String> = Vec::new();
fn main() {
    unsafe {
        g_extensions = vec![String::from("zip"), String::from("rar"), String::from("7z"), String::from("gz"), String::from("archive"), String::from("A##"), String::from("tar.bz2")];
                let _start_mem: i64 = _mem();
        let _start: i64 = _now();
        unsafe fn endsWith(s: &str, suf: &str) -> bool {
    if ((s.len() as i64) < (suf.len() as i64)) {
        return false
    }
    return ({ let tmp = &s; tmp.chars().skip(((s.len() as i64) - (suf.len() as i64)) as usize).take(((s.len() as i64) - ((s.len() as i64) - (suf.len() as i64))) as usize).collect::<String>() } == suf)
};
        unsafe fn lastIndexOf(s: &str, sub: &str) -> i64 {
    let mut idx: i64 = (0 - 1);
    let mut i: i64 = 0;
    while (i <= ((s.len() as i64) - (sub.len() as i64))) {
        if ({ let tmp = &s; tmp.chars().skip(i as usize).take(((i + (sub.len() as i64)) - i) as usize).collect::<String>() } == sub) {
            idx = i;
        }
        i = (i + 1);
    }
    return idx
};
        static mut g_extensions: Vec<String> = Vec::new();;
        unsafe fn fileExtInList(filename: &str) -> Ret {
    let mut fl: String = (filename.to_lowercase()).clone();
    for ext in g_extensions.clone().clone() {
        let mut ext2: String = format!("{}{}", ".", (ext.to_lowercase())).clone();
        if endsWith(&fl, &ext2) {
            return Ret {f0: true, f1: ext}
        }
    }
    let mut idx: i64 = lastIndexOf(filename, &".");
    if (idx != (0 - 1)) {
        let mut t: String = { let tmp = &filename; tmp.chars().skip((idx + 1) as usize).take(((filename.len() as i64) - (idx + 1)) as usize).collect::<String>() }.clone();
        if (t.as_str() != "") {
            return Ret {f0: false, f1: t}
        }
        return Ret {f0: false, f1: "<empty>"}
    }
    return Ret {f0: false, f1: "<none>"}
};
        unsafe fn pad(s: &str, mut w: i64) -> String {
    let mut t: String = s.to_string().clone();
    while ((t.len() as i64) < w) {
        t = format!("{}{}", t, " ");
    }
    return t.clone()
};
        unsafe fn mochi_main() {
    println!("{}", "The listed extensions are:");
    println!("{:?}", g_extensions.clone());
    let mut tests: Vec<String> = vec![String::from("MyData.a##"), String::from("MyData.tar.Gz"), String::from("MyData.gzip"), String::from("MyData.7z.backup"), String::from("MyData..."), String::from("MyData"), String::from("MyData_v1.0.tar.bz2"), String::from("MyData_v1.0.bz2")];
    for t in tests.clone() {
        let mut res: Ret = fileExtInList(&t);
        let mut ok: i64 = res.f0;
        let mut ext: String = res.f1.clone().to_string().clone();
        println!("{}", format!("{}{}", format!("{}{}", format!("{}{}", format!("{}{}", format!("{}{}", pad(&t, 20), " => "), ok.to_string()), "  (extension = "), ext), ")"));
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
