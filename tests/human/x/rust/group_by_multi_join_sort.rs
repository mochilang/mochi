#[derive(Clone)]
struct Nation {
    n_nationkey: i32,
    n_name: &'static str,
}

#[derive(Clone)]
struct Customer {
    c_custkey: i32,
    c_name: &'static str,
    c_acctbal: f64,
    c_nationkey: i32,
    c_address: &'static str,
    c_phone: &'static str,
    c_comment: &'static str,
}

#[derive(Clone)]
struct Order {
    o_orderkey: i32,
    o_custkey: i32,
    o_orderdate: &'static str,
}

#[derive(Clone)]
struct LineItem {
    l_orderkey: i32,
    l_returnflag: &'static str,
    l_extendedprice: f64,
    l_discount: f64,
}

fn main() {
    let nation = vec![Nation { n_nationkey: 1, n_name: "BRAZIL" }];
    let customer = vec![Customer {
        c_custkey: 1,
        c_name: "Alice",
        c_acctbal: 100.0,
        c_nationkey: 1,
        c_address: "123 St",
        c_phone: "123-456",
        c_comment: "Loyal",
    }];
    let orders = vec![
        Order { o_orderkey: 1000, o_custkey: 1, o_orderdate: "1993-10-15" },
        Order { o_orderkey: 2000, o_custkey: 1, o_orderdate: "1994-01-02" },
    ];
    let lineitem = vec![
        LineItem { l_orderkey: 1000, l_returnflag: "R", l_extendedprice: 1000.0, l_discount: 0.1 },
        LineItem { l_orderkey: 2000, l_returnflag: "N", l_extendedprice: 500.0, l_discount: 0.0 },
    ];

    let start_date = "1993-10-01";
    let end_date = "1994-01-01";

    use std::collections::HashMap;
    let mut groups: HashMap<i32, (String, f64, String, String, String, String, f64)> = HashMap::new();

    for c in &customer {
        for o in &orders {
            if o.o_custkey != c.c_custkey { continue; }
            if !(o.o_orderdate >= start_date && o.o_orderdate < end_date) { continue; }
            for l in &lineitem {
                if l.l_orderkey != o.o_orderkey { continue; }
                if l.l_returnflag != "R" { continue; }
                for n in &nation {
                    if n.n_nationkey != c.c_nationkey { continue; }
                    let revenue = l.l_extendedprice * (1.0 - l.l_discount);
                    let entry = groups.entry(c.c_custkey).or_insert((
                        c.c_name.to_string(),
                        c.c_acctbal,
                        c.c_address.to_string(),
                        c.c_phone.to_string(),
                        c.c_comment.to_string(),
                        n.n_name.to_string(),
                        0.0,
                    ));
                    entry.6 += revenue;
                }
            }
        }
    }

    let mut result: Vec<_> = groups.into_iter()
        .map(|(custkey, (name, acctbal, addr, phone, comment, n_name, revenue))| {
            (custkey, name, revenue, acctbal, n_name, addr, phone, comment)
        })
        .collect();

    result.sort_by(|a, b| b.2.partial_cmp(&a.2).unwrap());

    println!("{:?}", result);
}
