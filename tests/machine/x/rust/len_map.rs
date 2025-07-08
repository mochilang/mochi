fn main() {
    println!("{:?}", { let mut m = std::collections::HashMap::new(); m.insert("a", 1); m.insert("b", 2); m }.len());
}
