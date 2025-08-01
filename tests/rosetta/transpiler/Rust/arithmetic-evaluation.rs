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
#[derive(Debug, Clone, Default)]
struct Parser {
    expr: String,
    pos: i64,
}
impl std::fmt::Display for Parser {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        write!(f, "\"expr\": \"{}\"", self.expr)?;
        write!(f, ", ")?;
        write!(f, "\"pos\": {}", self.pos)?;
        write!(f, "}}")
    }
}

#[derive(Debug, Clone, Default)]
struct Res {
    v: i64,
    p: Parser,
}
impl std::fmt::Display for Res {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        write!(f, "\"v\": {}", self.v)?;
        write!(f, ", ")?;
        write!(f, "\"p\": {}", self.p)?;
        write!(f, "}}")
    }
}

fn main() {
        let _start_mem: i64 = _mem();
    let _start: i64 = _now();
    fn skipWS(mut p: Parser) -> Parser {
    let mut i: i64 = p.pos;
    while ((i < (p.expr.len() as i64)) && ({ let tmp = &p.expr; tmp.chars().skip(i as usize).take(((i + 1) - i) as usize).collect::<String>() }.as_str() == " ")) {
        i = (i + 1);
    }
    p.pos = i;
    return p
};
    fn parseIntStr(mut str: String) -> i64 {
    let mut i: i64 = 0;
    let mut n: i64 = 0;
    while (i < (str.len() as i64)) {
        n = (((n * 10) + { let n: i64 = str[i as usize..(i + 1) as usize].to_string().parse().unwrap(); n }) - 48);
        i = (i + 1);
    }
    return n
};
    fn parseNumber(mut p: Parser) -> Res {
    p = skipWS(p);
    let mut start: i64 = p.pos;
    while (p.pos < (p.expr.len() as i64)) {
        let mut ch: String = { let tmp = &p.expr; tmp.chars().skip(p.pos as usize).take(((p.pos + 1) - p.pos) as usize).collect::<String>() }.clone();
        if ((ch.as_str() >= "0") && (ch.as_str() <= "9")) {
            p.pos = (p.pos + 1);
        } else {
            break
        }
    }
    let mut token: String = { let tmp = &p.expr; tmp.chars().skip(start as usize).take((p.pos - start) as usize).collect::<String>() }.clone();
    return Res {v: parseIntStr(token.clone()), p: p}
};
    fn parseFactor(mut p: Parser) -> Res {
    p = skipWS(p);
    if ((p.pos < (p.expr.len() as i64)) && ({ let tmp = &p.expr; tmp.chars().skip(p.pos as usize).take(((p.pos + 1) - p.pos) as usize).collect::<String>() }.as_str() == "(")) {
        p.pos = (p.pos + 1);
        let mut r = parseExpr(p);
        let mut v: i64 = r.v;
        let mut p: i64 = r.p;
        let mut p: Parser = skipWS(p);
        if ((p.pos < (p.expr.len() as i64)) && ({ let tmp = &p.expr; tmp.chars().skip(p.pos as usize).take(((p.pos + 1) - p.pos) as usize).collect::<String>() }.as_str() == ")")) {
            p.pos = (p.pos + 1);
        }
        return Res {v: v, p: p}
    }
    if ((p.pos < (p.expr.len() as i64)) && ({ let tmp = &p.expr; tmp.chars().skip(p.pos as usize).take(((p.pos + 1) - p.pos) as usize).collect::<String>() }.as_str() == "-")) {
        p.pos = (p.pos + 1);
        let mut r: Res = parseFactor(p);
        let mut v: i64 = r.v;
        p = r.p;
        return Res {v: -v, p: p}
    }
    return parseNumber(p)
};
    fn powInt(mut base: i64, mut exp: i64) -> i64 {
    let mut r: i64 = 1;
    let mut b: i64 = base;
    let mut e: i64 = exp;
    while (e > 0) {
        if ((e % 2) == 1) {
            r = (r * b);
        }
        b = (b * b);
        e = (e / 2);
    }
    return r
};
    fn parsePower(mut p: Parser) -> Res {
    let mut r: Res = parseFactor(p);
    let mut v: i64 = r.v;
    p = r.p;
    loop {
        p = skipWS(p);
        if ((p.pos < (p.expr.len() as i64)) && ({ let tmp = &p.expr; tmp.chars().skip(p.pos as usize).take(((p.pos + 1) - p.pos) as usize).collect::<String>() }.as_str() == "^")) {
            p.pos = (p.pos + 1);
            let mut r2: Res = parseFactor(p);
            let mut rhs: i64 = r2.v;
            p = r2.p;
            v = powInt(v, rhs);
        } else {
            break
        }
    }
    return Res {v: v, p: p}
};
    fn parseTerm(mut p: Parser) -> Res {
    let mut r: Res = parsePower(p);
    let mut v: i64 = r.v;
    p = r.p;
    loop {
        p = skipWS(p);
        if (p.pos < (p.expr.len() as i64)) {
            let mut op: String = { let tmp = &p.expr; tmp.chars().skip(p.pos as usize).take(((p.pos + 1) - p.pos) as usize).collect::<String>() }.clone();
            if (op.as_str() == "*") {
                p.pos = (p.pos + 1);
                let mut r2: Res = parsePower(p);
                let mut rhs: i64 = r2.v;
                p = r2.p;
                v = (v * rhs);
                continue
            }
            if (op.as_str() == "/") {
                p.pos = (p.pos + 1);
                let mut r2: Res = parsePower(p);
                let mut rhs: i64 = r2.v;
                p = r2.p;
                v = (v / rhs);
                continue
            }
        }
        break
    }
    return Res {v: v, p: p}
};
    fn parseExpr(mut p: Parser) -> Res {
    let mut r: Res = parseTerm(p);
    let mut v: i64 = r.v;
    p = r.p;
    loop {
        p = skipWS(p);
        if (p.pos < (p.expr.len() as i64)) {
            let mut op: String = { let tmp = &p.expr; tmp.chars().skip(p.pos as usize).take(((p.pos + 1) - p.pos) as usize).collect::<String>() }.clone();
            if (op.as_str() == "+") {
                p.pos = (p.pos + 1);
                let mut r2: Res = parseTerm(p);
                let mut rhs: i64 = r2.v;
                p = r2.p;
                v = (v + rhs);
                continue
            }
            if (op.as_str() == "-") {
                p.pos = (p.pos + 1);
                let mut r2: Res = parseTerm(p);
                let mut rhs: i64 = r2.v;
                p = r2.p;
                v = (v - rhs);
                continue
            }
        }
        break
    }
    return Res {v: v, p: p}
};
    fn evalExpr(expr: &str) -> i64 {
    let mut p: Parser = Parser {expr: expr.to_string(), pos: 0};
    let mut r: Res = parseExpr(p);
    return r.v
};
    fn mochi_main() {
    let mut expr: String = String::from("2*(3-1)+2*5").clone();
    println!("{}", format!("{}{}", format!("{}{}", expr, " = "), evalExpr(&expr).to_string()));
};
    mochi_main();
    let _end: i64 = _now();
    let _end_mem: i64 = _mem();
    let duration_us: i64 = ((_end - _start) / 1000);
    let memory_bytes: i64 = (_end_mem - _start_mem);
    println!("{{\n  \"duration_us\": {},\n  \"memory_bytes\": {},\n  \"name\": \"{}\"\n}}", duration_us, memory_bytes, "main");

}
