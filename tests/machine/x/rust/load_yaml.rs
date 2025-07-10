#[derive(Default, Debug, Clone, PartialEq)]
struct Result {
    name: &'static str,
    email: &'static str,
}

fn _load<T: serde::de::DeserializeOwned>(path: &str, _opts: std::collections::HashMap<String, String>) -> Vec<T> {
    use std::io::Read;
    let mut data = String::new();
    if path.is_empty() || path == "-" {
        std::io::stdin().read_to_string(&mut data).unwrap();
    } else if let Ok(mut f) = std::fs::File::open(path) {
        f.read_to_string(&mut data).unwrap();
    }
    let fmt = _opts.get("format").map(String::as_str);
    if let Some("yaml") = fmt {
        if let Ok(v) = serde_yaml::from_str::<Vec<T>>(&data) { return v; }
        if let Ok(v) = serde_yaml::from_str::<T>(&data) { return vec![v]; }
    }
    if let Ok(v) = serde_json::from_str::<Vec<T>>(&data) { return v; }
    if let Ok(v) = serde_json::from_str::<T>(&data) { return vec![v]; }
    Vec::new()
}

fn main() {
    struct Person {
        name: &'static str,
        age: i32,
        email: &'static str,
    }
    let people = _load::<Person>("../interpreter/valid/people.yaml", { let mut m = std::collections::HashMap::new(); m.insert("format", "yaml"); m });
    let adults = { let mut tmp1 = Vec::new();for p in &people { if !(p.age >= 18) { continue; } tmp1.push(Result { name: p.name, email: p.email }); } tmp1 };
    for a in adults {
        println!("{:?} {:?}", a.name, a.email);
    }
}
