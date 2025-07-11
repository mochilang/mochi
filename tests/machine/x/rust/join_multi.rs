#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct Customer {
    id: i32,
    name: &'static str,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct Order {
    id: i32,
    customerId: i32,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct Item {
    orderId: i32,
    sku: &'static str,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct Result {
    name: &'static str,
    sku: &'static str,
}

fn main() {
    let customers = vec![Customer { id: 1, name: "Alice" }, Customer { id: 2, name: "Bob" }];
    let orders = vec![Order { id: 100, customerId: 1 }, Order { id: 101, customerId: 2 }];
    let items = vec![Item { orderId: 100, sku: "a" }, Item { orderId: 101, sku: "b" }];
    let result = { let mut tmp1 = Vec::new();for o in &orders { for c in &customers { if !(o.customerId == c.id) { continue; } for i in &items { if !(o.id == i.orderId) { continue; } tmp1.push(Result { name: c.name, sku: i.sku }); } } } tmp1 };
    println!("--- Multi Join ---");
    for r in result {
        println!("{} {} {}", r.name, "bought item", r.sku);
    }
}
