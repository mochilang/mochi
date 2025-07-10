#[derive(Default, Debug, Clone, PartialEq)]
struct Result {
    n: i32,
    l: &'static str,
    b: bool,
}

fn main() {
    let nums = vec![1, 2];
    let letters = vec!["A", "B"];
    let bools = vec![true, false];
    let combos = { let mut tmp1 = Vec::new();for &n in &nums { for &l in &letters { for &b in &bools { tmp1.push(Result { n: n, l: l, b: b }); } } } tmp1 };
    println!("{}", "--- Cross Join of three lists ---");
    for c in combos {
        println!("{:?} {:?} {:?}", c.n, c.l, c.b);
    }
}
