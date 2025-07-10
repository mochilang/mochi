#[derive(Default, Debug, Clone, PartialEq)]
struct Customer {
    id: i32,
    name: &'static str,
}

#[derive(Default, Debug, Clone, PartialEq)]
struct Order {
    id: i32,
    customerId: i32,
    total: i32,
}

#[derive(Default, Debug, Clone, PartialEq)]
struct Result {
    orderId: i32,
    customer: Customer,
    total: i32,
}

fn main() {
    let customers = vec![Customer { id: 1, name: "Alice" }, Customer { id: 2, name: "Bob" }];
    let orders = vec![Order { id: 100, customerId: 1, total: 250 }, Order { id: 101, customerId: 3, total: 80 }];
    let result = { let mut tmp1 = Vec::new();for o in &orders { let mut _matched = false; for c in &customers { if !(o.customerId == c.id) { continue; } _matched = true; tmp1.push(Result { orderId: o.id, customer: c.clone(), total: o.total }); } if !_matched { let c: Customer = Default::default(); tmp1.push(Result { orderId: o.id, customer: c.clone(), total: o.total }); } } tmp1 };
    println!("{}", "--- Left Join ---");
    for entry in result {
        println!("{} {:?} {} {:?} {} {:?}", "Order", entry.orderId, "customer", entry.customer, "total", entry.total);
    }
}
