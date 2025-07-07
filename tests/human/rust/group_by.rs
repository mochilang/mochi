use std::collections::BTreeMap;

#[derive(Clone)]
struct Person { name: &'static str, age: f64, city: &'static str }

fn main() {
    let people = vec![
        Person { name: "Alice", age: 30.0, city: "Paris" },
        Person { name: "Bob", age: 15.0, city: "Hanoi" },
        Person { name: "Charlie", age: 65.0, city: "Paris" },
        Person { name: "Diana", age: 45.0, city: "Hanoi" },
        Person { name: "Eve", age: 70.0, city: "Paris" },
        Person { name: "Frank", age: 22.0, city: "Hanoi" },
    ];

    let mut groups: BTreeMap<&str, Vec<f64>> = BTreeMap::new();
    for p in &people {
        groups.entry(p.city).or_default().push(p.age);
    }

    println!("--- People grouped by city ---");
    for (city, ages) in groups {
        let count = ages.len();
        let avg_age: f64 = ages.iter().sum::<f64>() / count as f64;
        println!("{} : count = {} , avg_age = {}", city, count, avg_age);
    }
}
