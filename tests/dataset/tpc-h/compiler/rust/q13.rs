// Generated by Mochi compiler v0.10.25 on 2025-07-13T17:34:44Z
#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Customer {
    c_custkey: i32,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Order {
    o_orderkey: i32,
    o_custkey: i32,
    o_comment: &'static str,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Result {
    c_count: i32,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Group {
    key: i32,
    items: Vec<Result>,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Result1 {
    c_count: i32,
    custdist: i32,
}

fn _json<T: std::fmt::Debug>(value: &T) {
    println!("{:?}", value);
}

fn main() {
    let customer = vec![Customer { c_custkey: 1 }, Customer { c_custkey: 2 }, Customer { c_custkey: 3 }];
    let orders = vec![Order { o_orderkey: 100, o_custkey: 1, o_comment: "fast delivery" }, Order { o_orderkey: 101, o_custkey: 1, o_comment: "no comment" }, Order { o_orderkey: 102, o_custkey: 2, o_comment: "special requests only" }];
    let per_customer = { let mut tmp2 = Vec::new();for c in &customer { tmp2.push(Result { c_count: { let mut tmp1 = Vec::new();for o in &orders { if !((o.o_custkey == c.c_custkey && (!(o.o_comment.contains("special"))) && (!(o.o_comment.contains("requests"))))) { continue; } tmp1.push(o.clone()); } tmp1 }.len() as i32 }); } tmp2 };
    let grouped = { let mut tmp3 = std::collections::HashMap::new();for x in &per_customer { let key = x.c_count; tmp3.entry(key).or_insert_with(Vec::new).push(x.clone()); } let mut tmp4 = Vec::<Group>::new(); for (k,v) in tmp3 { tmp4.push(Group { key: k, items: v }); } tmp4.sort_by(|a,b| a.key.partial_cmp(&b.key).unwrap()); tmp4.sort_by(|a,b| (-a.key).partial_cmp(&(-b.key)).unwrap()); let mut result = Vec::new(); for g in tmp4 { result.push(Result1 { c_count: g.key, custdist: g.clone().items.len() as i32 }); } result };
    _json(&grouped);
    assert!(grouped == vec![Result1 { c_count: 2, custdist: 1 }, Result1 { c_count: 0, custdist: 2 }]);
}
