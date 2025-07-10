#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct Nation {
    id: i32,
    name: &'static str,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct Supplier {
    id: i32,
    nation: i32,
}

#[derive(Default, Debug, Clone, PartialEq)]
struct Partsupp {
    part: i32,
    supplier: i32,
    cost: f64,
    qty: i32,
}

#[derive(Default, Debug, Clone, PartialEq)]
struct Result {
    part: i32,
    value: f64,
}

#[derive(Default, Debug, Clone, PartialEq)]
struct Group {
    key: i32,
    items: Vec<Result>,
}

#[derive(Default, Debug, Clone, PartialEq)]
struct Result1 {
    part: i32,
    total: f64,
}

fn sum<T>(v: &[T]) -> T where T: std::iter::Sum<T> + Copy {
    v.iter().copied().sum()
}

fn main() {
    let nations = vec![Nation { id: 1, name: "A" }, Nation { id: 2, name: "B" }];
    let suppliers = vec![Supplier { id: 1, nation: 1 }, Supplier { id: 2, nation: 2 }];
    let partsupp = vec![Partsupp { part: 100, supplier: 1, cost: 10.0, qty: 2 }, Partsupp { part: 100, supplier: 2, cost: 20.0, qty: 1 }, Partsupp { part: 200, supplier: 1, cost: 5.0, qty: 3 }];
    let filtered = { let mut tmp1 = Vec::new();for ps in &partsupp { for s in &suppliers { if !(s.id == ps.supplier) { continue; } for n in &nations { if !(n.id == s.nation) { continue; } if !(n.name == "A") { continue; } tmp1.push(Result { part: ps.part, value: ps.cost * ps.qty as f64 }); } } } tmp1 };
    let grouped = { let mut tmp2 = std::collections::HashMap::new();for x in &filtered { let key = x.part; tmp2.entry(key).or_insert_with(Vec::new).push(x.clone()); } let mut tmp3 = Vec::<Group>::new(); for (k,v) in tmp2 { tmp3.push(Group { key: k, items: v }); } let mut result = Vec::new(); for g in tmp3 { result.push(Result1 { part: g.key, total: sum(&{ let mut tmp4 = Vec::new();for r in &g.clone().items { tmp4.push(r.value); } tmp4 }) }); } result };
    println!("{:?}", grouped);
}
