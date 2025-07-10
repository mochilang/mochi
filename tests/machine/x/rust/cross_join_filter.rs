#[derive(Default, Debug, Clone, PartialEq)]
struct Result {
    n: i32,
    l: &'static str,
}

fn main() {
    let nums = vec![1, 2, 3];
    let letters = vec!["A", "B"];
    let pairs = { let mut tmp1 = Vec::new();for n in &nums { for &l in &letters { if !(n.clone() % 2 == 0) { continue; } tmp1.push(Result { n: n, l: l }); } } tmp1 };
    println!("{}", "--- Even pairs ---");
    for p in pairs {
        println!("{:?} {:?}", p.n, p.l);
    }
}
