//! Mochi Rust runtime: print, scalar conversion, and (in later phases)
//! collections, agents, streams, async, FFI, fetch, and LLM.

#![cfg_attr(feature = "embedded", no_std)]

#[cfg(feature = "embedded")]
extern crate alloc;

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
    //! Scalar conversions (Phase 2 onward).

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
    //! UTF-8 scalar string helpers matching Mochi semantics.

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
