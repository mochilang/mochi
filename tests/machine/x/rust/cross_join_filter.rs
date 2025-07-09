fn main() {
    let nums = vec![1, 2, 3];
    let letters = vec!["A", "B"];
    let pairs = { let mut tmp1 = Vec::new();for &n in &nums { for &l in &letters { if !(n % 2 == 0) { continue; } tmp1.push({ let mut m = std::collections::HashMap::new(); m.insert("n", n); m.insert("l", l); m }); } } tmp1 };
    println!("{:?}", "--- Even pairs ---");
    for p in pairs {
        println!("{:?} {:?}", p.n, p.l);
    }
}
