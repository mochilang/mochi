#[derive(Clone)]
struct Person { name: &'static str, age: i32 }

fn main() {
    let people = vec![
        Person { name: "Alice", age: 30 },
        Person { name: "Bob", age: 15 },
        Person { name: "Charlie", age: 65 },
        Person { name: "Diana", age: 45 },
    ];

    let adults: Vec<_> = people
        .into_iter()
        .filter(|p| p.age >= 18)
        .map(|p| (p.name, p.age, p.age >= 60))
        .collect();

    println!("--- Adults ---");
    for (name, age, is_senior) in adults {
        if is_senior {
            println!("{} is {}  (senior)", name, age);
        } else {
            println!("{} is {}", name, age);
        }
    }
}
