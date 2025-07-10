fn main() {
    let m = { let mut m = std::collections::BTreeMap::new(); m.insert(1, "a"); m.insert(2, "b"); m };
    println!("{}", m[&1]);
}
