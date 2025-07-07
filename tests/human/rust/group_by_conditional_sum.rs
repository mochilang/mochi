use std::collections::BTreeMap;

#[derive(Clone)]
struct Item { cat: &'static str, val: f64, flag: bool }

fn main() {
    let items = vec![
        Item { cat: "a", val: 10.0, flag: true },
        Item { cat: "a", val: 5.0, flag: false },
        Item { cat: "b", val: 20.0, flag: true },
    ];

    let mut groups: BTreeMap<&str, Vec<Item>> = BTreeMap::new();
    for i in items {
        groups.entry(i.cat).or_default().push(i);
    }

    for (cat, grp) in &groups {
        let total: f64 = grp.iter().map(|x| x.val).sum();
        let share_num: f64 = grp.iter().map(|x| if x.flag { x.val } else { 0.0 }).sum();
        let share = share_num / total;
        println!("map[cat:{} share:{}]", cat, share);
    }
}
