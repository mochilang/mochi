fn main() {
    let m = { let mut m = std::collections::HashMap::new(); m.insert(1, "a"); m.insert(2, "b"); m };
    println!("{:?}", m.contains_key(&1));
    println!("{:?}", m.contains_key(&3));
}
