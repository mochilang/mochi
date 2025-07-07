use std::collections::HashMap;

struct Nation { id: i32, name: &'static str }
struct Supplier { id: i32, nation: i32 }
struct PartSupp { part: i32, supplier: i32, cost: f64, qty: i32 }

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
        PartSupp { part: 100, supplier: 1, cost: 10.0, qty: 2 },
        PartSupp { part: 100, supplier: 2, cost: 20.0, qty: 1 },
        PartSupp { part: 200, supplier: 1, cost: 5.0, qty: 3 },
    ];
    let mut filtered: Vec<(i32, f64)> = Vec::new();
    for ps in &partsupp {
        if let Some(s) = suppliers.iter().find(|s| s.id == ps.supplier) {
            if let Some(n) = nations.iter().find(|n| n.id == s.nation) {
                if n.name == "A" {
                    filtered.push((ps.part, ps.cost * ps.qty as f64));
                }
            }
        }
    }
    let mut grouped: HashMap<i32, f64> = HashMap::new();
    for (part, value) in filtered {
        *grouped.entry(part).or_insert(0.0) += value;
    }
    let mut result: Vec<(i32, f64)> = grouped.into_iter().collect();
    result.sort_by(|a, b| a.0.cmp(&b.0));
    println!("[{:?}]", result);
}
