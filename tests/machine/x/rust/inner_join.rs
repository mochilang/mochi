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
    orderId: i32,
    customerName: &'static str,
    total: i32,
}

fn main() {
    let customers = vec![Customer { id: 1, name: "Alice" }, Customer { id: 2, name: "Bob" }, Customer { id: 3, name: "Charlie" }];
    let orders = vec![Order { id: 100, customerId: 1, total: 250 }, Order { id: 101, customerId: 2, total: 125 }, Order { id: 102, customerId: 1, total: 300 }, Order { id: 103, customerId: 4, total: 80 }];
    let result = { let mut tmp1 = Vec::new();for o in &orders { for c in &customers { if !(o.customerId == c.id) { continue; } tmp1.push(Result { orderId: o.id, customerName: c.name, total: o.total }); } } tmp1 };
    println!("{}", "--- Orders with customer info ---");
    for entry in result {
        println!("{} {:?} {} {:?} {} {:?}", "Order", entry.orderId, "by", entry.customerName, "- $", entry.total);
    }
}
