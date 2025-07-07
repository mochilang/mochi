struct Counter { n: i32 }
fn inc(c: &mut Counter) {
    c.n = c.n + 1;
}
fn main() {
    let mut c = Counter { n: 0 };
    inc(&mut c);
    println!("{}", c.n);
}
