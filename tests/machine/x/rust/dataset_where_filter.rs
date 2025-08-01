// Generated by Mochi compiler v0.10.28 on 1970-01-01T00:00:00Z
#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct People {
    name: &'static str,
    age: i32,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Result {
    name: &'static str,
    age: i32,
    is_senior: bool,
}

fn main() {
    let people = vec![People { name: "Alice", age: 30 }, People { name: "Bob", age: 15 }, People { name: "Charlie", age: 65 }, People { name: "Diana", age: 45 }];
    let adults = { let mut tmp1 = Vec::new();for person in &people { if !(person.age >= 18) { continue; } tmp1.push(Result { name: person.name, age: person.age, is_senior: person.age >= 60 }); } tmp1 };
    println!("--- Adults ---");
    for person in adults {
        println!("{}", vec![format!("{}", person.name), format!("{}", "is"), format!("{}", person.age), format!("{}", if person.is_senior { " (senior)" } else { "" })].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
    }
}
