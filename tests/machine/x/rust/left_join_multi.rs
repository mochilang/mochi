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
    orderId: i32,
    name: &'static str,
    item: Item,
}

fn main() {
    let customers = vec![Customer { id: 1, name: "Alice" }, Customer { id: 2, name: "Bob" }];
    let orders = vec![Order { id: 100, customerId: 1 }, Order { id: 101, customerId: 2 }];
    let items = vec![Item { orderId: 100, sku: "a" }];
    let result = { let mut tmp1 = Vec::new();for o in &orders { for c in &customers { if !(o.customerId == c.id) { continue; } let mut _matched = false; for i in &items { if !(o.id == i.orderId) { continue; } _matched = true; tmp1.push(Result { orderId: o.id, name: c.name, item: i.clone() }); } if !_matched { let i: Item = Default::default(); tmp1.push(Result { orderId: o.id, name: c.name, item: i.clone() }); } } } tmp1 };
    println!("{}", "--- Left Join Multi ---");
    for r in result {
        println!("{} {} {:?}", r.orderId, r.name, r.item);
    }
}
