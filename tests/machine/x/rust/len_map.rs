fn main() {
    println!("{}", { let mut m = std::collections::BTreeMap::new(); m.insert("a", 1); m.insert("b", 2); m }.len() as i32);
}
