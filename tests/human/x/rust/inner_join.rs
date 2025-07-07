struct Customer { id: i32, name: &'static str }
struct Order { id: i32, customer_id: i32, total: i32 }

fn main() {
    let customers = vec![
        Customer { id: 1, name: "Alice" },
        Customer { id: 2, name: "Bob" },
        Customer { id: 3, name: "Charlie" },
    ];
    let orders = vec![
        Order { id: 100, customer_id: 1, total: 250 },
        Order { id: 101, customer_id: 2, total: 125 },
        Order { id: 102, customer_id: 1, total: 300 },
        Order { id: 103, customer_id: 4, total: 80 },
    ];
    println!("--- Orders with customer info ---");
    for o in &orders {
        if let Some(c) = customers.iter().find(|c| c.id == o.customer_id) {
            println!("Order {} by {} - ${}", o.id, c.name, o.total);
        }
    }
}
