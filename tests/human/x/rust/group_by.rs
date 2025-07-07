use std::collections::HashMap;

struct Person { name: &'static str, age: i32, city: &'static str }

fn main() {
    let people = vec![
        Person { name: "Alice", age: 30, city: "Paris" },
        Person { name: "Bob", age: 15, city: "Hanoi" },
        Person { name: "Charlie", age: 65, city: "Paris" },
        Person { name: "Diana", age: 45, city: "Hanoi" },
        Person { name: "Eve", age: 70, city: "Paris" },
        Person { name: "Frank", age: 22, city: "Hanoi" },
    ];
    let mut map: HashMap<&str, Vec<i32>> = HashMap::new();
    for p in people {
        map.entry(p.city).or_default().push(p.age);
    }
    println!("--- People grouped by city ---");
    for (city, ages) in map {
        let count = ages.len();
        let avg: f64 = ages.iter().map(|&a| a as f64).sum::<f64>() / count as f64;
        println!("{}: count = {}, avg_age = {}", city, count, avg);
    }
}
