#[derive(Debug, Clone, PartialEq)]
struct Person { name: &'static str, age: i32, status: &'static str }

fn main() {
    let mut people = vec![
        Person { name: "Alice", age: 17, status: "minor" },
        Person { name: "Bob", age: 25, status: "unknown" },
        Person { name: "Charlie", age: 18, status: "unknown" },
        Person { name: "Diana", age: 16, status: "minor" },
    ];
    for p in &mut people {
        if p.age >= 18 {
            p.status = "adult";
            p.age += 1;
        }
    }
    let expected = vec![
        Person { name: "Alice", age: 17, status: "minor" },
        Person { name: "Bob", age: 26, status: "adult" },
        Person { name: "Charlie", age: 19, status: "adult" },
        Person { name: "Diana", age: 16, status: "minor" },
    ];
    assert_eq!(people, expected);
    println!("ok");
}
