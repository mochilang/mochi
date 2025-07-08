fn main() {
    let mut m = { let mut m = std::collections::HashMap::new(); m.insert("a", 1); m.insert("b", 2); m };
    for k in m {
        println!("{:?}", k);
    }
}
