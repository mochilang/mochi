use std::collections::HashMap;

struct Record { tag: &'static str, val: i32 }

fn main() {
    let data = vec![
        Record { tag: "a", val: 1 },
        Record { tag: "a", val: 2 },
        Record { tag: "b", val: 3 },
    ];
    let mut map: HashMap<&str, Vec<i32>> = HashMap::new();
    for d in &data {
        map.entry(d.tag).or_default().push(d.val);
    }
    let mut tmp: Vec<(&str, i32)> = Vec::new();
    for (tag, items) in map {
        let total: i32 = items.iter().sum();
        tmp.push((tag, total));
    }
    tmp.sort_by(|a, b| a.0.cmp(b.0));
    println!("[{:?}]", tmp);
}
