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
    ];
    println!("--- Left Join Multi ---");
    for o in &orders {
        if let Some(c) = customers.iter().find(|c| c.id == o.customer_id) {
            let item = items.iter().find(|i| i.order_id == o.id);
            let item_str = item.map(|i| i.sku).unwrap_or("<none>");
            println!("{} {} {}", o.id, c.name, item_str);
        }
    }
}
