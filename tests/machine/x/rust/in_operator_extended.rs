fn main() {
    let xs = vec![1, 2, 3];
    let ys = { let mut tmp1 = Vec::new();for &x in &xs { if !(x % 2 == 1) { continue; } tmp1.push(x); } tmp1 };
    println!("{}", ys.contains(&1));
    println!("{}", ys.contains(&2));
    let m = { let mut m = std::collections::BTreeMap::new(); m.insert("a", 1); m };
    println!("{:?}", m.contains_key(&"a"));
    println!("{:?}", m.contains_key(&"b"));
    let s = "hello";
    println!("{}", s.contains("ell"));
    println!("{}", s.contains("foo"));
}
