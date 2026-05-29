//! Mochi Rust runtime: print, scalar conversion, and (in later phases)
//! collections, agents, streams, async, FFI, fetch, and LLM.

#![cfg_attr(feature = "embedded", no_std)]

#[cfg(feature = "embedded")]
extern crate alloc;

pub mod io {
    //! Print helpers matching the vm3 print format.

    pub fn print_str(s: &str) {
        println!("{}", s);
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

    pub fn str_to_int(s: &str) -> i64 {
        s.parse::<i64>().unwrap_or(0)
    }

    pub fn int_to_str(n: i64) -> String {
        n.to_string()
    }
}

pub mod strings {
    //! UTF-8 scalar string helpers matching Mochi semantics.

    pub fn len(s: &str) -> i64 {
        s.chars().count() as i64
    }

    pub fn index(s: &str, i: i64) -> String {
        s.chars().nth(i as usize).map(|c| c.to_string()).unwrap_or_default()
    }

    pub fn contains(s: &str, sub: &str) -> bool {
        s.contains(sub)
    }
}
