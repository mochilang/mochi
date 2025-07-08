fn main() {
    let mut scores = { let mut m = std::collections::HashMap::new(); m.insert("alice", 1); m };
    scores["bob"] = 2;
    println!("{:?}", scores["bob"]);
}
