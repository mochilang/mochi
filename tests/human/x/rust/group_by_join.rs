use std::collections::HashMap;

struct Customer { id: i32, name: &'static str }
struct Order { id: i32, customer_id: i32 }

fn main() {
    let customers = vec![
        Customer { id: 1, name: "Alice" },
        Customer { id: 2, name: "Bob" },
    ];
    let orders = vec![
        Order { id: 100, customer_id: 1 },
        Order { id: 101, customer_id: 1 },
        Order { id: 102, customer_id: 2 },
    ];
    let mut map: HashMap<&str, usize> = HashMap::new();
    for o in &orders {
        if let Some(c) = customers.iter().find(|c| c.id == o.customer_id) {
            *map.entry(c.name).or_insert(0) += 1;
        }
    }
    println!("--- Orders per customer ---");
    for (name, count) in map {
        println!("{} orders: {}", name, count);
    }
}
