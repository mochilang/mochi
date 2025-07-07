use std::collections::HashMap;

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
    let mut map: HashMap<&str, usize> = HashMap::new();
    for p in people {
        *map.entry(p.city).or_insert(0) += 1;
    }
    let mut big: Vec<(&str, usize)> = map.into_iter().filter(|(_, c)| *c >= 4).collect();
    big.sort_by(|a, b| a.0.cmp(b.0));
    println!("[{:?}]", big);
}
