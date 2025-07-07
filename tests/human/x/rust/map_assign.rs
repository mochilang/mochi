use std::collections::HashMap;
fn main() {
    let mut scores: HashMap<&str, i32> = HashMap::new();
    scores.insert("alice", 1);
    scores.insert("bob", 2);
    println!("{}", scores.get("bob").unwrap());
}
