#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct Item {
    cat: &'static str,
    val: i32,
    flag: bool,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct Group {
    key: &'static str,
    items: Vec<Item>,
}

#[derive(Default, Debug, Clone, PartialEq)]
struct Result {
    cat: &'static str,
    share: f64,
}

fn sum<T>(v: &[T]) -> T where T: std::iter::Sum<T> + Copy {
    v.iter().copied().sum()
}

fn main() {
    let items = vec![Item { cat: "a", val: 10, flag: true }, Item { cat: "a", val: 5, flag: false }, Item { cat: "b", val: 20, flag: true }];
    let result = { let mut tmp1 = std::collections::HashMap::new();for i in &items { let key = i.cat; tmp1.entry(key).or_insert_with(Vec::new).push(i.clone()); } let mut tmp2 = Vec::<Group>::new(); for (k,v) in tmp1 { tmp2.push(Group { key: k, items: v }); } tmp2.sort_by(|a,b| a.key.partial_cmp(&b.key).unwrap()); tmp2.sort_by(|a,b| (a.key).partial_cmp(&(b.key)).unwrap()); let mut result = Vec::new(); for g in tmp2 { result.push(Result { cat: g.key, share: (sum(&{ let mut tmp3 = Vec::new();for x in &g.clone().items { tmp3.push(if x.flag != Default::default() { x.val } else { 0 }); } tmp3 }) as f64) / (sum(&{ let mut tmp4 = Vec::new();for x in &g.clone().items { tmp4.push(x.val); } tmp4 }) as f64) }); } result };
    println!("{:?}", result);
}
