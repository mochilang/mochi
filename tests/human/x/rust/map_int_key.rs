use std::collections::HashMap;
fn main() {
    let mut m: HashMap<i32, &str> = HashMap::new();
    m.insert(1, "a");
    m.insert(2, "b");
    println!("{}", m.get(&1).unwrap());
}
