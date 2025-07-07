use std::collections::HashMap;
fn main() {
    let x = 3;
    let y = 4;
    let mut m: HashMap<&str, i32> = HashMap::new();
    m.insert("a", x);
    m.insert("b", y);
    println!("{} {}", m.get("a").unwrap(), m.get("b").unwrap());
}
