fn main() {
    let items = vec![{ let mut m = std::collections::HashMap::new(); m.insert("n", 1); m.insert("v", "a"); m }, { let mut m = std::collections::HashMap::new(); m.insert("n", 1); m.insert("v", "b"); m }, { let mut m = std::collections::HashMap::new(); m.insert("n", 2); m.insert("v", "c"); m }];
    let result = { let mut tmp1 = Vec::new();for &i in &items { let tmp2 = i.v; let tmp3 = i.n; tmp1.push((tmp3, tmp2)); } tmp1.sort_by(|a,b| a.0.partial_cmp(&b.0).unwrap()); let mut tmp4 = Vec::new(); for p in tmp1 { tmp4.push(p.1); } tmp4 };
    println!("{:?}", result);
}
