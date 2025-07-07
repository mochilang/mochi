use std::collections::HashMap;

struct Item { cat: &'static str, val: i32 }

fn main() {
    let items = vec![
        Item { cat: "a", val: 3 },
        Item { cat: "a", val: 1 },
        Item { cat: "b", val: 5 },
        Item { cat: "b", val: 2 },
    ];
    let mut map: HashMap<&str, i32> = HashMap::new();
    for i in &items {
        *map.entry(i.cat).or_insert(0) += i.val;
    }
    let mut grouped: Vec<(&str, i32)> = map.into_iter().collect();
    grouped.sort_by(|a, b| b.1.cmp(&a.1));
    println!("[{:?}]", grouped);
}
