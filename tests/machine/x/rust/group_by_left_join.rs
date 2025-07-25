// Generated by Mochi compiler v0.10.28 on 1970-01-01T00:00:00Z
#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Customer {
    id: i32,
    name: &'static str,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Order {
    id: i32,
    customerId: i32,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Item {
    c: Customer,
    o: Order,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Group {
    key: &'static str,
    items: Vec<Item>,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Result {
    name: &'static str,
    count: i32,
}

fn main() {
    let customers = vec![Customer { id: 1, name: "Alice" }, Customer { id: 2, name: "Bob" }, Customer { id: 3, name: "Charlie" }];
    let orders = vec![Order { id: 100, customerId: 1 }, Order { id: 101, customerId: 1 }, Order { id: 102, customerId: 2 }];
    let stats = { let mut tmp1 = std::collections::HashMap::new();for c in &customers { let mut tmp3 = false; for o in &orders { if !(o.customerId == c.id) { continue; } tmp3 = true; let key = c.name; tmp1.entry(key).or_insert_with(Vec::new).push(Item {c: c.clone(), o: o.clone() }); } if !tmp3 { let o: Order = Default::default(); let key = c.name; tmp1.entry(key).or_insert_with(Vec::new).push(Item {c: c.clone(), o: o.clone() }); } } let mut tmp2 = Vec::<Group>::new(); for (k,v) in tmp1 { tmp2.push(Group { key: k, items: v }); } tmp2.sort_by(|a,b| a.key.partial_cmp(&b.key).unwrap()); let mut result = Vec::new(); for g in tmp2 { result.push(Result { name: g.key, count: { let mut tmp4 = Vec::new();for r in &g.clone().items { if !(r.o != Default::default()) { continue; } tmp4.push(r.clone()); } tmp4 }.len() as i32 }); } result };
    println!("--- Group Left Join ---");
    for s in stats {
        println!("{}", vec![format!("{}", s.name), format!("{}", "orders:"), format!("{}", s.count)].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
    }
}
