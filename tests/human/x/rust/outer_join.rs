struct Customer { id: i32, name: &'static str }
struct Order { id: i32, customer_id: i32, total: i32 }

fn main() {
    let customers = vec![
        Customer { id: 1, name: "Alice" },
        Customer { id: 2, name: "Bob" },
        Customer { id: 3, name: "Charlie" },
        Customer { id: 4, name: "Diana" },
    ];
    let orders = vec![
        Order { id: 100, customer_id: 1, total: 250 },
        Order { id: 101, customer_id: 2, total: 125 },
        Order { id: 102, customer_id: 1, total: 300 },
        Order { id: 103, customer_id: 5, total: 80 },
    ];
    println!("--- Outer Join using syntax ---");
    for o in &orders {
        if let Some(c) = customers.iter().find(|c| c.id == o.customer_id) {
            println!("Order {} by {} - ${}", o.id, c.name, o.total);
        } else {
            println!("Order {} by Unknown - ${}", o.id, o.total);
        }
    }
    for c in &customers {
        if !orders.iter().any(|o| o.customer_id == c.id) {
            println!("Customer {} has no orders", c.name);
        }
    }
}
