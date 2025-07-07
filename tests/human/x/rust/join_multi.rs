struct Customer { id: i32, name: &'static str }
struct Order { id: i32, customer_id: i32 }
struct Item { order_id: i32, sku: &'static str }

fn main() {
    let customers = vec![
        Customer { id: 1, name: "Alice" },
        Customer { id: 2, name: "Bob" },
    ];
    let orders = vec![
        Order { id: 100, customer_id: 1 },
        Order { id: 101, customer_id: 2 },
    ];
    let items = vec![
        Item { order_id: 100, sku: "a" },
        Item { order_id: 101, sku: "b" },
    ];
    println!("--- Multi Join ---");
    for o in &orders {
        if let Some(c) = customers.iter().find(|c| c.id == o.customer_id) {
            if let Some(i) = items.iter().find(|i| i.order_id == o.id) {
                println!("{} bought item {}", c.name, i.sku);
            }
        }
    }
}
