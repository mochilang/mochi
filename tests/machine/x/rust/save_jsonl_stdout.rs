#[derive(Default, Debug, Clone, PartialEq)]
struct People {
    name: &'static str,
    age: i32,
}

fn _save<T: serde::Serialize>(src: &[T], path: &str, _opts: std::collections::HashMap<String, String>) {
    if let Ok(text) = serde_json::to_string(src) {
        if path.is_empty() || path == "-" {
            println!("{}", text);
        } else {
            std::fs::write(path, text).unwrap();
        }
    }
}

fn main() {
    let people = vec![People { name: "Alice", age: 30 }, People { name: "Bob", age: 25 }];
    _save(people, "-", { let mut m = std::collections::HashMap::new(); m.insert("format", "jsonl"); m });
}
