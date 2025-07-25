// Generated by Mochi compiler v0.10.28 on 1970-01-01T00:00:00Z
#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Result {
    n: i32,
    l: &'static str,
}

fn main() {
    let nums = vec![1, 2, 3];
    let letters = vec!["A", "B"];
    let pairs = { let mut tmp1 = Vec::new();for &n in &nums { for &l in &letters { if !(n % 2 == 0) { continue; } tmp1.push(Result { n: n, l: l }); } } tmp1 };
    println!("--- Even pairs ---");
    for p in pairs {
        println!("{}", vec![format!("{}", p.n), format!("{}", p.l)].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
    }
}
