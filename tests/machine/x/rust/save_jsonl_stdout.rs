#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct People {
    name: &'static str,
    age: i32,
}

fn _save<T>(_src: &[T], _path: &str, _opts: std::collections::HashMap<String, String>) { }

fn main() {
    let people = vec![People { name: "Alice", age: 30 }, People { name: "Bob", age: 25 }];
    _save(people, "-", { let mut m = std::collections::BTreeMap::new(); m.insert("format", "jsonl"); m });
}
