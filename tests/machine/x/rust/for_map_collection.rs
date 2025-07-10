fn main() {
    let mut m = { let mut m = std::collections::BTreeMap::new(); m.insert("a", 1); m.insert("b", 2); m };
    for k in m.keys() {
        println!("{}", k);
    }
}
