struct Person { name: &'static str, age: i32 }

fn main() {
    let people = vec![
        Person { name: "Alice", age: 30 },
        Person { name: "Bob", age: 15 },
        Person { name: "Charlie", age: 65 },
        Person { name: "Diana", age: 45 },
    ];
    println!("--- Adults ---");
    for p in people.iter().filter(|p| p.age >= 18) {
        if p.age >= 60 {
            println!("{} is {} (senior)", p.name, p.age);
        } else {
            println!("{} is {}", p.name, p.age);
        }
    }
}
