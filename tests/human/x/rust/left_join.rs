struct Customer { id: i32, name: &'static str }
struct Order { id: i32, customer_id: i32, total: i32 }

fn main() {
    let customers = vec![
        Customer { id: 1, name: "Alice" },
        Customer { id: 2, name: "Bob" },
    ];
    let orders = vec![
        Order { id: 100, customer_id: 1, total: 250 },
        Order { id: 101, customer_id: 3, total: 80 },
    ];
    println!("--- Left Join ---");
    for o in &orders {
        let customer = customers.iter().find(|c| c.id == o.customer_id);
        let cust_str = match customer {
            Some(c) => c.name,
            None => "<none>",
        };
        println!("Order {} customer {} total {}", o.id, cust_str, o.total);
    }
}
