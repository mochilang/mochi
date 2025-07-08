fn main() {
    let data = vec![{ let mut m = std::collections::HashMap::new(); m.insert("a", 1); m.insert("b", 2); m }, { let mut m = std::collections::HashMap::new(); m.insert("a", 1); m.insert("b", 1); m }, { let mut m = std::collections::HashMap::new(); m.insert("a", 0); m.insert("b", 5); m }];
    let sorted = { let mut tmp1 = Vec::new();for x in &data { let tmp2 = x; let tmp3 = { let mut m = std::collections::HashMap::new(); m.insert("a", x.a); m.insert("b", x.b); m }; tmp1.push((tmp3, tmp2)); } tmp1.sort_by(|a,b| a.0.partial_cmp(&b.0).unwrap()); let mut tmp4 = Vec::new(); for p in tmp1 { tmp4.push(p.1); } tmp4 };
    println!("{:?}", sorted);
}
