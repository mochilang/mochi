use std::collections::HashMap;

struct Customer { id: i32, name: &'static str }
struct Order { id: i32, customer_id: i32 }

fn main() {
    let customers = vec![
        Customer { id: 1, name: "Alice" },
        Customer { id: 2, name: "Bob" },
        Customer { id: 3, name: "Charlie" },
    ];
    let orders = vec![
        Order { id: 100, customer_id: 1 },
        Order { id: 101, customer_id: 1 },
        Order { id: 102, customer_id: 2 },
    ];
    let mut map: HashMap<&str, usize> = HashMap::new();
    for c in &customers {
        let count = orders.iter().filter(|o| o.customer_id == c.id).count();
        map.insert(c.name, count);
    }
    println!("--- Group Left Join ---");
    for (name, count) in map {
        println!("{} orders: {}", name, count);
    }
}
