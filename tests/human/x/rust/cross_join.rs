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
    ];
    println!("--- Cross Join: All order-customer pairs ---");
    for o in &orders {
        for c in &customers {
            println!(
                "Order {} (customerId: {}, total: ${}) paired with {}",
                o.id, o.customer_id, o.total, c.name
            );
        }
    }
}
