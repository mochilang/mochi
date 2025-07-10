fn main() {
    let mut scores = { let mut m = std::collections::BTreeMap::new(); m.insert("alice", 1); m };
    scores.insert("bob", 2);
    println!("{}", scores[&"bob"]);
}
