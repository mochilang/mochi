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
    order: Order,
    customer: Customer,
}

fn main() {
    let customers = vec![Customer { id: 1, name: "Alice" }, Customer { id: 2, name: "Bob" }, Customer { id: 3, name: "Charlie" }, Customer { id: 4, name: "Diana" }];
    let orders = vec![Order { id: 100, customerId: 1, total: 250 }, Order { id: 101, customerId: 2, total: 125 }, Order { id: 102, customerId: 1, total: 300 }, Order { id: 103, customerId: 5, total: 80 }];
    let result = { let mut tmp1 = Vec::new();for o in &orders { let mut _matched = false; for c in &customers { if !(o.customerId == c.id) { continue; } _matched = true; tmp1.push(Result { order: o.clone(), customer: c.clone() }); } if !_matched { let c: Customer = Default::default(); tmp1.push(Result { order: o.clone(), customer: c.clone() }); } } for c in &customers { let mut _matched = false; for o in &orders { if o.customerId == c.id { _matched = true; break; } } if !_matched { let o: Order = Default::default(); tmp1.push(Result { order: o.clone(), customer: c.clone() }); } } tmp1 };
    println!("{}", "--- Outer Join using syntax ---");
    for row in result {
        if row.order != Order::default() {
            if row.customer != Customer::default() {
                println!("{} {} {} {} {} {}", "Order", row.order.id, "by", row.customer.name, "- $", row.order.total);
            } else {
                println!("{} {} {} {} {} {}", "Order", row.order.id, "by", "Unknown", "- $", row.order.total);
            }
        } else {
            println!("{} {} {}", "Customer", row.customer.name, "has no orders");
        }
    }
}
