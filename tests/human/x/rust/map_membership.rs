use std::collections::HashMap;
fn main() {
    let mut m: HashMap<&str, i32> = HashMap::new();
    m.insert("a", 1);
    m.insert("b", 2);
    println!("{}", m.contains_key("a"));
    println!("{}", m.contains_key("c"));
}
