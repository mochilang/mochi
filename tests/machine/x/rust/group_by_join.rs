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
    o: Order,
    c: Customer,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct Group {
    key: &'static str,
    items: Vec<Item>,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct Result {
    name: &'static str,
    count: i32,
}

fn main() {
    let customers = vec![Customer { id: 1, name: "Alice" }, Customer { id: 2, name: "Bob" }];
    let orders = vec![Order { id: 100, customerId: 1 }, Order { id: 101, customerId: 1 }, Order { id: 102, customerId: 2 }];
    let stats = { let mut tmp1 = std::collections::HashMap::new();for o in &orders { for c in &customers { if !(o.customerId == c.id) { continue; } let key = c.name; tmp1.entry(key).or_insert_with(Vec::new).push(Item {o: o.clone(), c: c.clone() }); } } let mut tmp2 = Vec::<Group>::new(); for (k,v) in tmp1 { tmp2.push(Group { key: k, items: v }); } let mut result = Vec::new(); for g in tmp2 { result.push(Result { name: g.key, count: g.clone().items.len() as i32 }); } result };
    println!("{}", "--- Orders per customer ---");
    for s in stats {
        println!("{} {} {}", s.name, "orders:", s.count);
    }
}
