#[derive(Default, Debug, Clone, PartialEq)]
struct Data {
    tag: &'static str,
    val: i32,
}

#[derive(Default, Debug, Clone, PartialEq)]
struct Group {
    key: &'static str,
    items: Vec<Data>,
}

fn append<T: Clone>(mut v: Vec<T>, item: T) -> Vec<T> {
    v.push(item);
    v
}

fn main() {
    let data = vec![Data { tag: "a", val: 1 }, Data { tag: "a", val: 2 }, Data { tag: "b", val: 3 }];
    let groups = { let mut tmp1 = std::collections::HashMap::new();for d in &data { let key = d.tag; tmp1.entry(key).or_insert_with(Vec::new).push(d.clone()); } let mut tmp2 = Vec::<Group>::new(); for (k,v) in tmp1 { tmp2.push(Group { key: k, items: v }); } let mut result = Vec::new(); for g in tmp2 { result.push(g.clone()); } result };
    let mut tmp = vec![];
    for g in groups {
        let mut total = 0;
        for x in g.items {
            total = total + x.val;
        }
        tmp = append(tmp, { let mut m = std::collections::HashMap::new(); m.insert("tag", g.key); m.insert("total", total); m });
    }
    let result = { let mut tmp3 = Vec::new();for &r in &tmp { let tmp4 = r; let tmp5 = r.tag; tmp3.push((tmp5, tmp4)); } tmp3.sort_by(|a,b| a.0.partial_cmp(&b.0).unwrap()); let mut tmp6 = Vec::new(); for p in tmp3 { tmp6.push(p.1); } tmp6 };
    println!("{:?}", result);
}
