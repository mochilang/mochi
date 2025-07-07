use std::collections::BTreeMap;

#[derive(Clone)]
struct Customer { id: i32, name: &'static str }
#[derive(Clone)]
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

    let mut groups: BTreeMap<&str, i32> = BTreeMap::new();
    for c in &customers {
        let count = orders.iter().filter(|o| o.customer_id == c.id).count() as i32;
        groups.insert(c.name, count);
    }

    println!("--- Group Left Join ---");
    for (name, count) in groups {
        println!("{} orders: {}", name, count);
    }
}
