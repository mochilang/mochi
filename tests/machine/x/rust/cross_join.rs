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
    orderCustomerId: i32,
    pairedCustomerName: &'static str,
    orderTotal: i32,
}

fn main() {
    let customers = vec![Customer { id: 1, name: "Alice" }, Customer { id: 2, name: "Bob" }, Customer { id: 3, name: "Charlie" }];
    let orders = vec![Order { id: 100, customerId: 1, total: 250 }, Order { id: 101, customerId: 2, total: 125 }, Order { id: 102, customerId: 1, total: 300 }];
    let result = { let mut tmp1 = Vec::new();for o in &orders { for c in &customers { tmp1.push(Result { orderId: o.id, orderCustomerId: o.customerId, pairedCustomerName: c.name, orderTotal: o.total }); } } tmp1 };
    println!("--- Cross Join: All order-customer pairs ---");
    for entry in result {
        println!("{} {} {} {} {} {} {} {}", "Order", entry.orderId, "(customerId:", entry.orderCustomerId, ", total: $", entry.orderTotal, ") paired with", entry.pairedCustomerName);
    }
}
