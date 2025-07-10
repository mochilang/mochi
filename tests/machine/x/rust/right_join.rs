#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct Customer {
    id: i32,
    name: &'static str,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct Order {
    id: i32,
    customerId: i32,
    total: i32,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct Result {
    customerName: &'static str,
    order: Order,
}

fn main() {
    let customers = vec![Customer { id: 1, name: "Alice" }, Customer { id: 2, name: "Bob" }, Customer { id: 3, name: "Charlie" }, Customer { id: 4, name: "Diana" }];
    let orders = vec![Order { id: 100, customerId: 1, total: 250 }, Order { id: 101, customerId: 2, total: 125 }, Order { id: 102, customerId: 1, total: 300 }];
    let result = { let mut tmp1 = Vec::new();for o in &orders { let mut _matched = false; for c in &customers { if !(o.customerId == c.id) { continue; } _matched = true; tmp1.push(Result { customerName: c.name, order: o.clone() }); } if !_matched { let c: Customer = Default::default(); tmp1.push(Result { customerName: c.name, order: o.clone() }); } } tmp1 };
    println!("{}", "--- Right Join using syntax ---");
    for entry in result {
        if entry.order != Order::default() {
            println!("{} {} {} {} {} {}", "Customer", entry.customerName, "has order", entry.order.id, "- $", entry.order.total);
        } else {
            println!("{} {} {}", "Customer", entry.customerName, "has no orders");
        }
    }
}
