//! Mochi Rust runtime: print, scalar conversion, and (in later phases)
//! collections, agents, streams, async, FFI, fetch, and LLM.
//!
//! Phase 18 (`embedded` feature, `no_std` + `alloc`): only the modules
//! `conv` and `strings` are exported. The remaining modules require
//! `std` and are gated behind `cfg(feature = "std")`.

#![cfg_attr(feature = "embedded", no_std)]

#[cfg(feature = "embedded")]
extern crate alloc;

#[cfg(feature = "std")]
pub mod io {
    //! Print helpers matching the vm3 print format.

    pub fn print_str<S: AsRef<str>>(s: S) {
        println!("{}", s.as_ref());
    }

    pub fn print_i64(n: i64) {
        println!("{}", n);
    }

    pub fn print_f64(f: f64) {
        if f.is_nan() {
            println!("NaN");
            return;
        }
        if f.is_infinite() {
            println!("{}", if f > 0.0 { "+Inf" } else { "-Inf" });
            return;
        }
        if f.fract() == 0.0 && f >= -9007199254740992.0 && f <= 9007199254740992.0 {
            println!("{}", f as i64);
            return;
        }
        println!("{}", f);
    }

    pub fn print_bool(b: bool) {
        println!("{}", if b { "true" } else { "false" });
    }
}

pub mod conv {
    //! Scalar conversions (Phase 2 onward). no_std-compatible (needs alloc).

    #[cfg(feature = "embedded")]
    use alloc::string::{String, ToString};

    pub fn int_to_float(n: i64) -> f64 {
        n as f64
    }

    pub fn float_to_int(f: f64) -> i64 {
        f as i64
    }

    pub fn str_to_int<S: AsRef<str>>(s: S) -> i64 {
        s.as_ref().parse::<i64>().unwrap_or(0)
    }

    pub fn int_to_str(n: i64) -> String {
        n.to_string()
    }
}

pub mod strings {
    //! UTF-8 scalar string helpers matching Mochi semantics. no_std-compatible (needs alloc).

    #[cfg(feature = "embedded")]
    use alloc::string::{String, ToString};

    pub fn len<S: AsRef<str>>(s: S) -> i64 {
        s.as_ref().chars().count() as i64
    }

    pub fn index<S: AsRef<str>>(s: S, i: i64) -> String {
        s.as_ref().chars().nth(i as usize).map(|c| c.to_string()).unwrap_or_default()
    }

    pub fn contains<S: AsRef<str>, T: AsRef<str>>(s: S, sub: T) -> bool {
        s.as_ref().contains(sub.as_ref())
    }

    pub fn cat<S: AsRef<str>, T: AsRef<str>>(a: S, b: T) -> String {
        let mut out = String::with_capacity(a.as_ref().len() + b.as_ref().len());
        out.push_str(a.as_ref());
        out.push_str(b.as_ref());
        out
    }

    pub fn substring<S: AsRef<str>>(s: S, start: i64, end: i64) -> String {
        let s = s.as_ref();
        let mut iter = s.chars();
        let mut out = String::new();
        let mut i: i64 = 0;
        while i < end {
            match iter.next() {
                Some(c) => {
                    if i >= start {
                        out.push(c);
                    }
                }
                None => break,
            }
            i += 1;
        }
        out
    }

    pub fn reverse<S: AsRef<str>>(s: S) -> String {
        s.as_ref().chars().rev().collect()
    }
}

#[cfg(feature = "std")]
pub mod chan {
    use std::cell::RefCell;
    use std::collections::VecDeque;
    use std::rc::Rc;

    pub struct Chan<T> {
        inner: Rc<RefCell<VecDeque<T>>>,
    }

    impl<T> Chan<T> {
        pub fn make(_cap: i64) -> Self {
            Self { inner: Rc::new(RefCell::new(VecDeque::new())) }
        }

        pub fn send(&self, v: T) {
            self.inner.borrow_mut().push_back(v);
        }

        pub fn recv(&self) -> T {
            self.inner.borrow_mut().pop_front().expect("recv on empty chan")
        }
    }

    impl<T> Clone for Chan<T> {
        fn clone(&self) -> Self {
            Self { inner: Rc::clone(&self.inner) }
        }
    }

    impl<T> std::fmt::Debug for Chan<T> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str("Chan(..)")
        }
    }
}

#[cfg(feature = "std")]
pub mod stream {
    use std::cell::RefCell;
    use std::collections::VecDeque;
    use std::rc::Rc;

    pub struct Stream<T> {
        subs: Rc<RefCell<Vec<Rc<RefCell<VecDeque<T>>>>>>,
    }

    impl<T: Clone> Stream<T> {
        pub fn make(_cap: i64) -> Self {
            Self { subs: Rc::new(RefCell::new(Vec::new())) }
        }

        pub fn emit(&self, v: T) {
            for s in self.subs.borrow().iter() {
                s.borrow_mut().push_back(v.clone());
            }
        }
    }

    impl<T> Clone for Stream<T> {
        fn clone(&self) -> Self {
            Self { subs: Rc::clone(&self.subs) }
        }
    }

    impl<T> std::fmt::Debug for Stream<T> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str("Stream(..)")
        }
    }

    pub struct Sub<T> {
        inner: Rc<RefCell<VecDeque<T>>>,
    }

    impl<T> Sub<T> {
        pub fn recv(&self) -> T {
            self.inner.borrow_mut().pop_front().expect("recv on empty sub")
        }
    }

    impl<T> Clone for Sub<T> {
        fn clone(&self) -> Self {
            Self { inner: Rc::clone(&self.inner) }
        }
    }

    impl<T> std::fmt::Debug for Sub<T> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str("Sub(..)")
        }
    }

    pub fn subscribe<T>(s: &Stream<T>) -> Sub<T> {
        let q = Rc::new(RefCell::new(VecDeque::new()));
        s.subs.borrow_mut().push(Rc::clone(&q));
        Sub { inner: q }
    }

    pub fn subscribe_limit<T>(s: &Stream<T>, _limit: i64) -> Sub<T> {
        subscribe(s)
    }
}

#[cfg(feature = "std")]
pub mod panic {
    use std::panic;
    use std::sync::Once;

    static SILENCE_HOOK: Once = Once::new();

    pub fn silence_hook() {
        SILENCE_HOOK.call_once(|| {
            panic::set_hook(Box::new(|_| {}));
        });
    }

    pub fn raise(code: i64) -> ! {
        panic::panic_any(code);
    }

    pub fn catch<F: FnOnce()>(f: F) -> Option<i64> {
        silence_hook();
        match panic::catch_unwind(panic::AssertUnwindSafe(f)) {
            Ok(()) => None,
            Err(p) => Some(payload_to_code(&p)),
        }
    }

    fn payload_to_code(p: &Box<dyn std::any::Any + Send>) -> i64 {
        if let Some(&code) = p.downcast_ref::<i64>() {
            return code;
        }
        if let Some(s) = p.downcast_ref::<&'static str>() {
            return map_msg(s);
        }
        if let Some(s) = p.downcast_ref::<String>() {
            return map_msg(s.as_str());
        }
        1
    }

    fn map_msg(s: &str) -> i64 {
        if s.contains("out of bounds") || s.contains("index out of") || s.contains("index ") {
            return 4;
        }
        if s.contains("divide by zero") || s.contains("attempt to divide") || s.contains("remainder") {
            return 5;
        }
        1
    }
}

#[cfg(feature = "std")]
pub mod fetch {
    //! Minimal HTTP/1.1 GET client using std::net::TcpStream. Phase 14.
    //!
    //! Only supports plain http:// URLs (no TLS) which is enough for the
    //! Phase 14 httptest harness. Returns the response body verbatim.
    use std::io::{Read, Write};
    use std::net::TcpStream;

    pub fn get<U: AsRef<str>>(url: U) -> String {
        let url = url.as_ref();
        let (host, port, path) = match parse_url(url) {
            Some(t) => t,
            None => super::panic::raise(98),
        };
        let addr = format!("{}:{}", host, port);
        let mut stream = match TcpStream::connect(&addr) {
            Ok(s) => s,
            Err(_) => super::panic::raise(98),
        };
        let req = format!(
            "GET {} HTTP/1.1\r\nHost: {}\r\nConnection: close\r\nUser-Agent: mochi-rust/0.1\r\n\r\n",
            path, host_header(&host, port)
        );
        if stream.write_all(req.as_bytes()).is_err() {
            super::panic::raise(98);
        }
        let mut buf = Vec::new();
        if stream.read_to_end(&mut buf).is_err() {
            super::panic::raise(98);
        }
        let split = match find_header_end(&buf) {
            Some(i) => i,
            None => super::panic::raise(98),
        };
        let header_str = match std::str::from_utf8(&buf[..split]) {
            Ok(s) => s,
            Err(_) => super::panic::raise(98),
        };
        if let Some(status) = parse_status(header_str) {
            if status >= 400 {
                super::panic::raise(98);
            }
        }
        let body = &buf[split + 4..];
        if is_chunked(header_str) {
            return match decode_chunked(body) {
                Some(s) => s,
                None => super::panic::raise(98),
            };
        }
        match std::str::from_utf8(body) {
            Ok(s) => s.to_string(),
            Err(_) => super::panic::raise(98),
        }
    }

    fn host_header(host: &str, port: u16) -> String {
        if port == 80 { host.to_string() } else { format!("{}:{}", host, port) }
    }

    fn parse_url(url: &str) -> Option<(String, u16, String)> {
        let rest = url.strip_prefix("http://")?;
        let (host_part, path_part) = match rest.find('/') {
            Some(i) => (&rest[..i], &rest[i..]),
            None => (rest, "/"),
        };
        let (host, port) = match host_part.rfind(':') {
            Some(i) => {
                let p: u16 = host_part[i + 1..].parse().ok()?;
                (host_part[..i].to_string(), p)
            }
            None => (host_part.to_string(), 80u16),
        };
        Some((host, port, path_part.to_string()))
    }

    fn find_header_end(buf: &[u8]) -> Option<usize> {
        for i in 0..buf.len().saturating_sub(3) {
            if &buf[i..i + 4] == b"\r\n\r\n" {
                return Some(i);
            }
        }
        None
    }

    fn parse_status(headers: &str) -> Option<u16> {
        let line = headers.lines().next()?;
        let mut parts = line.split_whitespace();
        parts.next()?;
        parts.next()?.parse().ok()
    }

    fn is_chunked(headers: &str) -> bool {
        for line in headers.lines().skip(1) {
            let lower = line.to_ascii_lowercase();
            if let Some(v) = lower.strip_prefix("transfer-encoding:") {
                if v.trim() == "chunked" {
                    return true;
                }
            }
        }
        false
    }

    fn decode_chunked(body: &[u8]) -> Option<String> {
        let mut out = Vec::new();
        let mut i = 0;
        while i < body.len() {
            let mut j = i;
            while j < body.len() && body[j] != b'\r' {
                j += 1;
            }
            let size_str = std::str::from_utf8(&body[i..j]).ok()?;
            let size = usize::from_str_radix(size_str.trim(), 16).ok()?;
            if j + 2 > body.len() {
                return None;
            }
            i = j + 2;
            if size == 0 {
                break;
            }
            if i + size > body.len() {
                return None;
            }
            out.extend_from_slice(&body[i..i + size]);
            i += size + 2;
        }
        String::from_utf8(out).ok()
    }
}

#[cfg(feature = "std")]
pub mod json {
    //! Minimal JSON object decoder. Returns HashMap<String, String> matching
    //! the Mochi `json_decode` contract: top-level object with non-string
    //! values coerced to their string representation. Phase 14.
    use std::collections::HashMap;

    pub fn decode<S: AsRef<str>>(input: S) -> HashMap<String, String> {
        let s = input.as_ref();
        let mut out = HashMap::new();
        let bytes = s.as_bytes();
        let mut i = skip_ws(bytes, 0);
        if i >= bytes.len() || bytes[i] != b'{' {
            super::panic::raise(97);
        }
        i += 1;
        loop {
            i = skip_ws(bytes, i);
            if i < bytes.len() && bytes[i] == b'}' {
                return out;
            }
            let (k, n1) = match parse_string(bytes, i) {
                Some(t) => t,
                None => super::panic::raise(97),
            };
            i = skip_ws(bytes, n1);
            if i >= bytes.len() || bytes[i] != b':' {
                super::panic::raise(97);
            }
            i = skip_ws(bytes, i + 1);
            let (v, n2) = match parse_value(bytes, i) {
                Some(t) => t,
                None => super::panic::raise(97),
            };
            out.insert(k, v);
            i = skip_ws(bytes, n2);
            if i < bytes.len() && bytes[i] == b',' {
                i += 1;
                continue;
            }
            if i < bytes.len() && bytes[i] == b'}' {
                return out;
            }
            super::panic::raise(97);
        }
    }

    fn skip_ws(bytes: &[u8], mut i: usize) -> usize {
        while i < bytes.len() {
            match bytes[i] {
                b' ' | b'\t' | b'\r' | b'\n' => i += 1,
                _ => break,
            }
        }
        i
    }

    fn parse_string(bytes: &[u8], i: usize) -> Option<(String, usize)> {
        if i >= bytes.len() || bytes[i] != b'"' {
            return None;
        }
        let mut out = String::new();
        let mut j = i + 1;
        while j < bytes.len() {
            match bytes[j] {
                b'"' => return Some((out, j + 1)),
                b'\\' => {
                    if j + 1 >= bytes.len() {
                        return None;
                    }
                    match bytes[j + 1] {
                        b'"' => out.push('"'),
                        b'\\' => out.push('\\'),
                        b'/' => out.push('/'),
                        b'n' => out.push('\n'),
                        b'r' => out.push('\r'),
                        b't' => out.push('\t'),
                        b'b' => out.push('\x08'),
                        b'f' => out.push('\x0c'),
                        _ => return None,
                    }
                    j += 2;
                }
                c => {
                    out.push(c as char);
                    j += 1;
                }
            }
        }
        None
    }

    fn parse_value(bytes: &[u8], i: usize) -> Option<(String, usize)> {
        if i >= bytes.len() {
            return None;
        }
        match bytes[i] {
            b'"' => parse_string(bytes, i),
            b't' if bytes[i..].starts_with(b"true") => Some(("true".to_string(), i + 4)),
            b'f' if bytes[i..].starts_with(b"false") => Some(("false".to_string(), i + 5)),
            b'n' if bytes[i..].starts_with(b"null") => Some(("".to_string(), i + 4)),
            b'-' | b'0'..=b'9' => parse_number(bytes, i),
            _ => None,
        }
    }

    fn parse_number(bytes: &[u8], i: usize) -> Option<(String, usize)> {
        let mut j = i;
        if bytes[j] == b'-' {
            j += 1;
        }
        while j < bytes.len() {
            match bytes[j] {
                b'0'..=b'9' | b'.' | b'e' | b'E' | b'+' | b'-' => j += 1,
                _ => break,
            }
        }
        if j == i {
            return None;
        }
        Some((std::str::from_utf8(&bytes[i..j]).ok()?.to_string(), j))
    }
}

#[cfg(feature = "std")]
pub mod llm {
    use std::env;
    use std::fs;
    use std::path::PathBuf;

    pub fn call<P: AsRef<str>, Q: AsRef<str>>(provider: P, prompt: Q) -> String {
        let dir = match env::var("MOCHI_LLM_CASSETTE_DIR") {
            Ok(d) => d,
            Err(_) => super::panic::raise(99),
        };
        let key = format!("{}:{}", provider.as_ref(), prompt.as_ref());
        let hex = sha256_hex(key.as_bytes());
        let path = PathBuf::from(dir).join(format!("{}.txt", hex));
        match fs::read_to_string(&path) {
            Ok(s) => trim_trailing(&s),
            Err(_) => super::panic::raise(99),
        }
    }

    fn trim_trailing(s: &str) -> String {
        let mut end = s.len();
        let bytes = s.as_bytes();
        while end > 0 {
            let c = bytes[end - 1];
            if c == b'\n' || c == b'\r' || c == b' ' || c == b'\t' {
                end -= 1;
            } else {
                break;
            }
        }
        s[..end].to_string()
    }

    fn sha256_hex(input: &[u8]) -> String {
        let h = sha256(input);
        let mut out = String::with_capacity(64);
        for b in h.iter() {
            out.push_str(&format!("{:02x}", b));
        }
        out
    }

    const K: [u32; 64] = [
        0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
        0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
        0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
        0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
        0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
        0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
        0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
        0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
    ];

    fn sha256(input: &[u8]) -> [u8; 32] {
        let mut h: [u32; 8] = [
            0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
            0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19,
        ];
        let bit_len = (input.len() as u64).wrapping_mul(8);
        let mut padded = Vec::with_capacity(input.len() + 72);
        padded.extend_from_slice(input);
        padded.push(0x80);
        while padded.len() % 64 != 56 {
            padded.push(0);
        }
        padded.extend_from_slice(&bit_len.to_be_bytes());

        for chunk in padded.chunks(64) {
            let mut w = [0u32; 64];
            for i in 0..16 {
                w[i] = u32::from_be_bytes([chunk[i * 4], chunk[i * 4 + 1], chunk[i * 4 + 2], chunk[i * 4 + 3]]);
            }
            for i in 16..64 {
                let s0 = w[i - 15].rotate_right(7) ^ w[i - 15].rotate_right(18) ^ (w[i - 15] >> 3);
                let s1 = w[i - 2].rotate_right(17) ^ w[i - 2].rotate_right(19) ^ (w[i - 2] >> 10);
                w[i] = w[i - 16].wrapping_add(s0).wrapping_add(w[i - 7]).wrapping_add(s1);
            }

            let mut a = h[0];
            let mut b = h[1];
            let mut c = h[2];
            let mut d = h[3];
            let mut e = h[4];
            let mut f = h[5];
            let mut g = h[6];
            let mut hh = h[7];

            for i in 0..64 {
                let s1 = e.rotate_right(6) ^ e.rotate_right(11) ^ e.rotate_right(25);
                let ch = (e & f) ^ ((!e) & g);
                let t1 = hh.wrapping_add(s1).wrapping_add(ch).wrapping_add(K[i]).wrapping_add(w[i]);
                let s0 = a.rotate_right(2) ^ a.rotate_right(13) ^ a.rotate_right(22);
                let mj = (a & b) ^ (a & c) ^ (b & c);
                let t2 = s0.wrapping_add(mj);

                hh = g;
                g = f;
                f = e;
                e = d.wrapping_add(t1);
                d = c;
                c = b;
                b = a;
                a = t1.wrapping_add(t2);
            }

            h[0] = h[0].wrapping_add(a);
            h[1] = h[1].wrapping_add(b);
            h[2] = h[2].wrapping_add(c);
            h[3] = h[3].wrapping_add(d);
            h[4] = h[4].wrapping_add(e);
            h[5] = h[5].wrapping_add(f);
            h[6] = h[6].wrapping_add(g);
            h[7] = h[7].wrapping_add(hh);
        }

        let mut out = [0u8; 32];
        for i in 0..8 {
            out[i * 4..i * 4 + 4].copy_from_slice(&h[i].to_be_bytes());
        }
        out
    }
}

#[cfg(feature = "std")]
pub mod check {
    use super::panic::raise;

    pub fn div_i64(a: i64, b: i64) -> i64 {
        if b == 0 {
            raise(5);
        }
        a / b
    }

    pub fn mod_i64(a: i64, b: i64) -> i64 {
        if b == 0 {
            raise(5);
        }
        a % b
    }

    pub fn list_index<T: Clone>(xs: &[T], i: i64) -> T {
        if i < 0 || (i as usize) >= xs.len() {
            raise(4);
        }
        xs[i as usize].clone()
    }
}
