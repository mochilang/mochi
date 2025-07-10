fn main() {
    let m = { let mut m = std::collections::HashMap::new(); m.insert("a", 1); m.insert("b", 2); m };
    println!("{}", m.contains_key(&"a"));
    println!("{}", m.contains_key(&"c"));
}
