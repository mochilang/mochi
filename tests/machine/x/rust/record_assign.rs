fn main() {
    struct Counter {
        n: i32,
    }
    fn inc(c: Counter) -> () {
        c.n = c.n + 1;
    }
    let mut c = Counter { n: 0 };
    inc(c);
    println!("{:?}", c.n);
}
