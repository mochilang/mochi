#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct People {
    name: &'static str,
    city: &'static str,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct Group {
    key: &'static str,
    items: Vec<People>,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct Result {
    city: &'static str,
    num: i32,
}

fn main() {
    let people = vec![People { name: "Alice", city: "Paris" }, People { name: "Bob", city: "Hanoi" }, People { name: "Charlie", city: "Paris" }, People { name: "Diana", city: "Hanoi" }, People { name: "Eve", city: "Paris" }, People { name: "Frank", city: "Hanoi" }, People { name: "George", city: "Paris" }];
    let big = { let mut tmp1 = std::collections::HashMap::new();for p in &people { let key = p.city; tmp1.entry(key).or_insert_with(Vec::new).push(p.clone()); } let mut tmp2 = Vec::<Group>::new(); for (k,v) in tmp1 { tmp2.push(Group { key: k, items: v }); } tmp2.sort_by(|a,b| a.key.partial_cmp(&b.key).unwrap()); let mut tmp3 = Vec::new(); for g in tmp2.into_iter() { if g.clone().items.len() as i32 >= 4 { tmp3.push(g); } } let mut result = Vec::new(); for g in tmp3 { result.push(Result { city: g.key, num: g.clone().items.len() as i32 }); } result };
    println!("{:?}", big);
}
