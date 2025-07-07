use std::collections::BTreeMap;

fn main() {
    let mut m = BTreeMap::new();
    m.insert("a", 1);
    m.insert("b", 2);
    for k in m.keys() {
        println!("{}", k);
    }
}
