use std::collections::BTreeMap;
use serde_json::json;

#[derive(Clone)]
struct Nation { n_nationkey: i32, n_name: &'static str }
#[derive(Clone)]
struct Customer { c_custkey: i32, c_name: &'static str, c_acctbal: f64, c_nationkey: i32, c_address: &'static str, c_phone: &'static str, c_comment: &'static str }
#[derive(Clone)]
struct Order { o_orderkey: i32, o_custkey: i32, o_orderdate: &'static str }
#[derive(Clone)]
struct LineItem { l_orderkey: i32, l_returnflag: &'static str, l_extendedprice: f64, l_discount: f64 }

fn main() {
    let nation = vec![Nation { n_nationkey: 1, n_name: "BRAZIL" }];
    let customer = vec![Customer { c_custkey: 1, c_name: "Alice", c_acctbal: 100.0, c_nationkey: 1, c_address: "123 St", c_phone: "123-456", c_comment: "Loyal" }];
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

    let mut groups: BTreeMap<i32, (serde_json::Value, f64)> = BTreeMap::new();

    for c in &customer {
        for o in &orders {
            if o.o_custkey == c.c_custkey && o.o_orderdate >= start_date && o.o_orderdate < end_date {
                for l in &lineitem {
                    if l.l_orderkey == o.o_orderkey && l.l_returnflag == "R" {
                        for n in &nation {
                            if n.n_nationkey == c.c_nationkey {
                                let key = c.c_custkey;
                                let revenue = l.l_extendedprice * (1.0 - l.l_discount);
                                let entry = groups.entry(key).or_insert((json!({
                                    "c_custkey": c.c_custkey,
                                    "c_name": c.c_name,
                                    "c_acctbal": c.c_acctbal,
                                    "c_address": c.c_address,
                                    "c_phone": c.c_phone,
                                    "c_comment": c.c_comment,
                                    "n_name": n.n_name
                                }), 0.0));
                                entry.1 += revenue;
                            }
                        }
                    }
                }
            }
        }
    }

    let mut results: Vec<_> = groups.into_iter().map(|(_, (info, rev))| {
        json!({
            "c_custkey": info["c_custkey"],
            "c_name": info["c_name"],
            "revenue": rev,
            "c_acctbal": info["c_acctbal"],
            "n_name": info["n_name"],
            "c_address": info["c_address"],
            "c_phone": info["c_phone"],
            "c_comment": info["c_comment"]
        })
    }).collect();

    results.sort_by(|a,b| b["revenue"].as_f64().partial_cmp(&a["revenue"].as_f64()).unwrap());

    println!("{}", serde_json::to_string(&results).unwrap());
}
