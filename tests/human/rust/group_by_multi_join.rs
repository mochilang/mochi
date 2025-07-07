use std::collections::BTreeMap;

#[derive(Clone)]
struct Nation { id: i32, name: &'static str }
#[derive(Clone)]
struct Supplier { id: i32, nation: i32 }
#[derive(Clone)]
struct PartSupp { part: i32, supplier: i32, cost: f64, qty: f64 }

fn main() {
    let nations = vec![
        Nation { id: 1, name: "A" },
        Nation { id: 2, name: "B" },
    ];
    let suppliers = vec![
        Supplier { id: 1, nation: 1 },
        Supplier { id: 2, nation: 2 },
    ];
    let partsupp = vec![
        PartSupp { part: 100, supplier: 1, cost: 10.0, qty: 2.0 },
        PartSupp { part: 100, supplier: 2, cost: 20.0, qty: 1.0 },
        PartSupp { part: 200, supplier: 1, cost: 5.0, qty: 3.0 },
    ];

    let mut filtered = Vec::new();
    for ps in &partsupp {
        if let Some(s) = suppliers.iter().find(|s| s.id == ps.supplier) {
            if let Some(n) = nations.iter().find(|n| n.id == s.nation && n.name == "A") {
                let value = ps.cost * ps.qty;
                filtered.push((ps.part, value));
            }
        }
    }

    let mut groups: BTreeMap<i32, f64> = BTreeMap::new();
    for (part, value) in filtered {
        *groups.entry(part).or_insert(0.0) += value;
    }

    for (part, total) in groups {
        println!("map[part:{} total:{}]", part, total);
    }
}
