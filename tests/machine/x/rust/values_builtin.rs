fn main() {
    let m = { let mut m = std::collections::HashMap::new(); m.insert("a", 1); m.insert("b", 2); m.insert("c", 3); m };
    println!("{:?}", m.values().cloned().collect::<Vec<_>>());
}
