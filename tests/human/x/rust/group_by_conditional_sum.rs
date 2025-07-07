use std::collections::HashMap;

struct Item { cat: &'static str, val: i32, flag: bool }

fn main() {
    let items = vec![
        Item { cat: "a", val: 10, flag: true },
        Item { cat: "a", val: 5, flag: false },
        Item { cat: "b", val: 20, flag: true },
    ];
    let mut map: HashMap<&str, Vec<&Item>> = HashMap::new();
    for item in &items {
        map.entry(item.cat).or_default().push(item);
    }
    let mut result: Vec<(&str, f64)> = Vec::new();
    for (cat, vec) in map {
        let total: i32 = vec.iter().map(|i| i.val).sum();
        let selected: i32 = vec.iter().map(|i| if i.flag { i.val } else { 0 }).sum();
        result.push((cat, selected as f64 / total as f64));
    }
    result.sort_by(|a, b| a.0.cmp(b.0));
    println!("[{:?}]", result);
}
