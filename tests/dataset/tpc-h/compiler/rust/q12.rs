// Generated by Mochi compiler v0.10.25 on 2025-07-13T17:34:43Z
#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Order {
    o_orderkey: i32,
    o_orderpriority: &'static str,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Lineitem {
    l_orderkey: i32,
    l_shipmode: &'static str,
    l_commitdate: &'static str,
    l_receiptdate: &'static str,
    l_shipdate: &'static str,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Item {
    l: Lineitem,
    o: Order,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Group {
    key: &'static str,
    items: Vec<Item>,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Result {
    l_shipmode: &'static str,
    high_line_count: i32,
    low_line_count: i32,
}

fn sum<T>(v: &[T]) -> T where T: std::iter::Sum<T> + Copy {
    v.iter().copied().sum()
}

fn _json<T: std::fmt::Debug>(value: &T) {
    println!("{:?}", value);
}

fn main() {
    let orders = vec![Order { o_orderkey: 1, o_orderpriority: "1-URGENT" }, Order { o_orderkey: 2, o_orderpriority: "3-MEDIUM" }];
    let lineitem = vec![Lineitem { l_orderkey: 1, l_shipmode: "MAIL", l_commitdate: "1994-02-10", l_receiptdate: "1994-02-15", l_shipdate: "1994-02-05" }, Lineitem { l_orderkey: 2, l_shipmode: "SHIP", l_commitdate: "1994-03-01", l_receiptdate: "1994-02-28", l_shipdate: "1994-02-27" }];
    let result = { let mut tmp1 = std::collections::HashMap::new();for l in &lineitem { for o in &orders { if !(o.o_orderkey == l.l_orderkey) { continue; } if !((vec!["MAIL", "SHIP"].contains(&l.l_shipmode)) && (l.l_commitdate < l.l_receiptdate) && (l.l_shipdate < l.l_commitdate) && (l.l_receiptdate >= "1994-01-01") && (l.l_receiptdate < "1995-01-01")) { continue; } let key = l.l_shipmode; tmp1.entry(key).or_insert_with(Vec::new).push(Item {l: l.clone(), o: o.clone() }); } } let mut tmp2 = Vec::<Group>::new(); for (k,v) in tmp1 { tmp2.push(Group { key: k, items: v }); } tmp2.sort_by(|a,b| a.key.partial_cmp(&b.key).unwrap()); tmp2.sort_by(|a,b| (a.key).partial_cmp(&(b.key)).unwrap()); let mut result = Vec::new(); for g in tmp2 { result.push(Result { l_shipmode: g.key, high_line_count: sum(&{ let mut tmp3 = Vec::new();for x in &g.clone().items { tmp3.push(if vec!["1-URGENT", "2-HIGH"].contains(&x.o.o_orderpriority) { 1 } else { 0 }); } tmp3 }), low_line_count: sum(&{ let mut tmp4 = Vec::new();for x in &g.clone().items { tmp4.push(if !(vec!["1-URGENT", "2-HIGH"].contains(&x.o.o_orderpriority)) { 1 } else { 0 }); } tmp4 }) }); } result };
    _json(&result);
    assert!(result == vec![Result { l_shipmode: "MAIL", high_line_count: 1, low_line_count: 0 }]);
}
