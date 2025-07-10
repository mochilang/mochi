#[derive(Default, Debug, Clone, PartialEq)]
struct Item {
    cat: &'static str,
    val: i32,
}

#[derive(Default, Debug, Clone, PartialEq)]
struct Group {
    key: &'static str,
    items: Vec<Item>,
}

#[derive(Default, Debug, Clone, PartialEq)]
struct Result {
    cat: &'static str,
    total: i32,
}

fn sum(v: &[i32]) -> i32 {
    v.iter().sum()
}

fn main() {
    let items = vec![Item { cat: "a", val: 3 }, Item { cat: "a", val: 1 }, Item { cat: "b", val: 5 }, Item { cat: "b", val: 2 }];
    let grouped = { let mut tmp1 = std::collections::HashMap::new();for i in &items { let key = i.cat; tmp1.entry(key).or_insert_with(Vec::new).push(i.clone()); } let mut tmp2 = Vec::<Group>::new(); for (k,v) in tmp1 { tmp2.push(Group { key: k, items: v }); } tmp2.sort_by(|a,b| (-sum(&{ let mut tmp3 = Vec::new();for x in &a.clone().items { tmp3.push(x.val); } tmp3 })).partial_cmp(&(-sum(&{ let mut tmp3 = Vec::new();for x in &b.clone().items { tmp3.push(x.val); } tmp3 }))).unwrap()); let mut result = Vec::new(); for g in tmp2 { result.push(Result { cat: g.key, total: sum(&{ let mut tmp4 = Vec::new();for x in &g.clone().items { tmp4.push(x.val); } tmp4 }) }); } result };
    println!("{:?}", grouped);
}
