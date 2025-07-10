#[derive(Default, Debug, Clone, PartialEq)]
struct People {
    name: &'static str,
    age: i32,
}

#[derive(Default, Debug, Clone, PartialEq)]
struct Result {
    name: &'static str,
    age: i32,
    is_senior: bool,
}

fn main() {
    let people = vec![People { name: "Alice", age: 30 }, People { name: "Bob", age: 15 }, People { name: "Charlie", age: 65 }, People { name: "Diana", age: 45 }];
    let adults = { let mut tmp1 = Vec::new();for person in &people { if !(person.age >= 18) { continue; } tmp1.push(Result { name: person.name, age: person.age, is_senior: person.age >= 60 }); } tmp1 };
    println!("{}", "--- Adults ---");
    for person in adults {
        println!("{:?} {} {:?} {}", person.name, "is", person.age, if person.is_senior != Default::default() { " (senior)" } else { "" });
    }
}
