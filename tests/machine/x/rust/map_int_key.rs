// Generated by Mochi compiler v0.10.28 on 1970-01-01T00:00:00Z
fn main() {
    let m = { let mut m = std::collections::BTreeMap::new(); m.insert(1, "a"); m.insert(2, "b"); m };
    println!("{}", vec![format!("{}", *m.get(&1).unwrap())].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
}
