use std::collections::BTreeMap;

#[derive(Clone)]
struct Person { name: &'static str, city: &'static str }

fn main() {
    let people = vec![
        Person { name: "Alice", city: "Paris" },
        Person { name: "Bob", city: "Hanoi" },
        Person { name: "Charlie", city: "Paris" },
        Person { name: "Diana", city: "Hanoi" },
        Person { name: "Eve", city: "Paris" },
        Person { name: "Frank", city: "Hanoi" },
        Person { name: "George", city: "Paris" },
    ];

    let mut groups: BTreeMap<&str, Vec<&Person>> = BTreeMap::new();
    for p in &people {
        groups.entry(p.city).or_default().push(p);
    }

    let mut big = Vec::new();
    for (city, group) in &groups {
        if group.len() >= 4 {
            big.push((city, group.len()));
        }
    }

    let json_output = serde_json::to_string(&big.iter().map(|(city, num)| serde_json::json!({"city": city, "num": num})).collect::<Vec<_>>()).unwrap();
    println!("{}", json_output);
}
