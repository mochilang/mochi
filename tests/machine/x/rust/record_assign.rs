#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct Counter {
        n: i32,
}

fn main() {
    fn inc(c: &mut Counter) -> () {
        c.n = c.n + 1;
    }
    let mut c = Counter { n: 0 };
    inc(&mut c.clone());
    println!("{}", c.n);
}
