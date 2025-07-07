use std::collections::BTreeMap;

#[derive(Clone)]
struct Item { cat: &'static str, val: i32 }

fn main() {
    let items = vec![
        Item { cat: "a", val: 3 },
        Item { cat: "a", val: 1 },
        Item { cat: "b", val: 5 },
        Item { cat: "b", val: 2 },
    ];

    let mut groups: BTreeMap<&str, Vec<i32>> = BTreeMap::new();
    for i in items {
        groups.entry(i.cat).or_default().push(i.val);
    }

    let mut results: Vec<(String, i32)> = groups.into_iter().map(|(cat, vals)| {
        let total: i32 = vals.iter().sum();
        (cat.to_string(), total)
    }).collect();

    results.sort_by(|a,b| b.1.cmp(&a.1));

    for (cat, total) in results {
        println!("map[cat:{} total:{}]", cat, total);
    }
}
