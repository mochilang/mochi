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
struct Point {
    x: f64,
    y: f64,
}
impl std::fmt::Display for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        write!(f, "\"x\": {}", self.x)?;
        write!(f, ", ")?;
        write!(f, "\"y\": {}", self.y)?;
        write!(f, "}}")
    }
}

#[derive(Debug, Clone, Default)]
struct Line {
    slope: f64,
    yint: f64,
}
impl std::fmt::Display for Line {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        write!(f, "\"slope\": {}", self.slope)?;
        write!(f, ", ")?;
        write!(f, "\"yint\": {}", self.yint)?;
        write!(f, "}}")
    }
}

fn main() {
        let _start_mem: i64 = _mem();
    let _start: i64 = _now();
    fn createLine(a: &Point, b: &Point) -> Line {
    let mut slope: f64 = ((b.y - a.y) / (b.x - a.x));
    let mut yint: f64 = (a.y - (slope * a.x));
    return Line {slope: slope, yint: yint}
};
    fn evalX(l: &Line, mut x: f64) -> f64 {
    return ((l.slope * x) + l.yint)
};
    fn intersection(l1: &Line, l2: &Line) -> Point {
    if (l1.slope == l2.slope) {
        return Point {x: 0.0, y: 0.0}
    }
    let mut x: f64 = ((l2.yint - l1.yint) / (l1.slope - l2.slope));
    let mut y: f64 = evalX(l1, x);
    return Point {x: x, y: y}
};
    fn mochi_main() {
    let mut l1: Line = createLine(&Point {x: 4.0, y: 0.0}, &Point {x: 6.0, y: 10.0});
    let mut l2: Line = createLine(&Point {x: 0.0, y: 3.0}, &Point {x: 10.0, y: 7.0});
    let mut p: Point = intersection(&l1, &l2);
    println!("{}", format!("{}{}", format!("{}{}", format!("{}{}", format!("{}{}", "{", p.x.to_string()), " "), p.y.to_string()), "}"));
};
    mochi_main();
    let _end: i64 = _now();
    let _end_mem: i64 = _mem();
    let duration_us: i64 = ((_end - _start) / 1000);
    let memory_bytes: i64 = (_end_mem - _start_mem);
    println!("{{\n  \"duration_us\": {},\n  \"memory_bytes\": {},\n  \"name\": \"{}\"\n}}", duration_us, memory_bytes, "main");

}
