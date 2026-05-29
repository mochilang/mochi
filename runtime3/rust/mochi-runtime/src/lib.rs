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
