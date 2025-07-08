fn main() {
    let mut x = 3;
    let mut y = 4;
    let mut m = { let mut m = std::collections::HashMap::new(); m.insert("a", x); m.insert("b", y); m };
    println!("{:?} {:?}", m["a"], m["b"]);
}
