#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct Nation {
    n_nationkey: i32,
    n_name: &'static str,
}

#[derive(Default, Debug, Clone, PartialEq)]
struct Customer {
    c_custkey: i32,
    c_name: &'static str,
    c_acctbal: f64,
    c_nationkey: i32,
    c_address: &'static str,
    c_phone: &'static str,
    c_comment: &'static str,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct Order {
    o_orderkey: i32,
    o_custkey: i32,
    o_orderdate: &'static str,
}

#[derive(Default, Debug, Clone, PartialEq)]
struct Lineitem {
    l_orderkey: i32,
    l_returnflag: &'static str,
    l_extendedprice: f64,
    l_discount: f64,
}

#[derive(Default, Debug, Clone, PartialEq)]
struct Key {
    c_custkey: i32,
    c_name: &'static str,
    c_acctbal: f64,
    c_address: &'static str,
    c_phone: &'static str,
    c_comment: &'static str,
    n_name: &'static str,
}

#[derive(Default, Debug, Clone, PartialEq)]
struct Item {
    c: Customer,
    o: Order,
    l: Lineitem,
    n: Nation,
}

#[derive(Default, Debug, Clone, PartialEq)]
struct Group {
    key: Key,
    items: Vec<Item>,
}

#[derive(Default, Debug, Clone, PartialEq)]
struct Result {
    c_custkey: i32,
    c_name: &'static str,
    revenue: i32,
    c_acctbal: f64,
    n_name: &'static str,
    c_address: &'static str,
    c_phone: &'static str,
    c_comment: &'static str,
}

fn sum<T>(v: &[T]) -> T where T: std::iter::Sum<T> + Copy {
    v.iter().copied().sum()
}

fn main() {
    let nation = vec![Nation { n_nationkey: 1, n_name: "BRAZIL" }];
    let customer = vec![Customer { c_custkey: 1, c_name: "Alice", c_acctbal: 100.0, c_nationkey: 1, c_address: "123 St", c_phone: "123-456", c_comment: "Loyal" }];
    let orders = vec![Order { o_orderkey: 1000, o_custkey: 1, o_orderdate: "1993-10-15" }, Order { o_orderkey: 2000, o_custkey: 1, o_orderdate: "1994-01-02" }];
    let lineitem = vec![Lineitem { l_orderkey: 1000, l_returnflag: "R", l_extendedprice: 1000.0, l_discount: 0.1 }, Lineitem { l_orderkey: 2000, l_returnflag: "N", l_extendedprice: 500.0, l_discount: 0.0 }];
    let start_date = "1993-10-01";
    let end_date = "1994-01-01";
    let result = { let mut tmp1 = std::collections::HashMap::new();for c in &customer { for o in &orders { if !(o.o_custkey == c.c_custkey) { continue; } for l in &lineitem { if !(l.l_orderkey == o.o_orderkey) { continue; } for n in &nation { if !(n.n_nationkey == c.c_nationkey) { continue; } if !(o.o_orderdate >= start_date && o.o_orderdate < end_date && l.l_returnflag == "R") { continue; } let key = Key { c_custkey: c.c_custkey, c_name: c.c_name, c_acctbal: c.c_acctbal, c_address: c.c_address, c_phone: c.c_phone, c_comment: c.c_comment, n_name: n.n_name }; tmp1.entry(key).or_insert_with(Vec::new).push(Item {c: c.clone(), o: o.clone(), l: l.clone(), n: n.clone() }); } } } } let mut tmp2 = Vec::<Group>::new(); for (k,v) in tmp1 { tmp2.push(Group { key: k, items: v }); } tmp2.sort_by(|a,b| (-sum(&{ let mut tmp3 = Vec::new();for x in &a.clone().items { tmp3.push(x.l.l_extendedprice * (1 - x.l.l_discount)); } tmp3 })).partial_cmp(&(-sum(&{ let mut tmp3 = Vec::new();for x in &b.clone().items { tmp3.push(x.l.l_extendedprice * (1 - x.l.l_discount)); } tmp3 }))).unwrap()); let mut result = Vec::new(); for g in tmp2 { result.push(Result { c_custkey: g.key.c_custkey, c_name: g.key.c_name, revenue: sum(&{ let mut tmp4 = Vec::new();for x in &g.clone().items { tmp4.push(x.l.l_extendedprice * (1 - x.l.l_discount)); } tmp4 }), c_acctbal: g.key.c_acctbal, n_name: g.key.n_name, c_address: g.key.c_address, c_phone: g.key.c_phone, c_comment: g.key.c_comment }); } result };
    println!("{:?}", result);
}
