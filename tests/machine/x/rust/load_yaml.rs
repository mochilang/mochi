#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct Person {
        name: &'static str,
        age: i32,
        email: &'static str,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct Result {
    name: &'static str,
    email: &'static str,
}

fn _load<T: Default + Clone>(_path: &str, _opts: std::collections::HashMap<String, String>) -> Vec<T> {
    Vec::new()
}

fn main() {
    let people = _load::<Person>("../interpreter/valid/people.yaml", { let mut m = std::collections::BTreeMap::new(); m.insert("format", "yaml"); m });
    let adults = { let mut tmp1 = Vec::new();for p in &people { if !(p.age >= 18) { continue; } tmp1.push(Result { name: p.name, email: p.email }); } tmp1 };
    for a in adults {
        println!("{} {}", a.name, a.email);
    }
}
