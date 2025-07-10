#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct People {
    name: &'static str,
    age: i32,
}

fn _save<T: serde::Serialize>(src: &[T], path: &str, opts: std::collections::HashMap<String, String>) {
    if let Some(fmt) = opts.get("format") {
        if fmt == "jsonl" {
            if path.is_empty() || path == "-" {
                for item in src { if let Ok(text) = serde_json::to_string(item) { println!("{}", text); } }
            } else if let Ok(mut f) = std::fs::File::create(path) {
                use std::io::Write;
                for item in src { if let Ok(text) = serde_json::to_string(item) { writeln!(f, "{}", text).unwrap(); } }
            }
            return;
        }
    }
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
    _save(people, "-", { let mut m = std::collections::BTreeMap::new(); m.insert("format", "jsonl"); m });
}
