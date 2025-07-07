use std::collections::BTreeMap;
fn main() {
    let mut m = BTreeMap::new();
    m.insert("a", 1);
    m.insert("b", 2);
    m.insert("c", 3);
    let values: Vec<_> = m.values().copied().collect();
    for (i, v) in values.iter().enumerate() {
        if i > 0 { print!(" "); }
        print!("{}", v);
    }
    println!();
}
