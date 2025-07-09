fn main() {
    let products = vec![{ let mut m = std::collections::HashMap::new(); m.insert("name", "Laptop"); m.insert("price", 1500); m }, { let mut m = std::collections::HashMap::new(); m.insert("name", "Smartphone"); m.insert("price", 900); m }, { let mut m = std::collections::HashMap::new(); m.insert("name", "Tablet"); m.insert("price", 600); m }, { let mut m = std::collections::HashMap::new(); m.insert("name", "Monitor"); m.insert("price", 300); m }, { let mut m = std::collections::HashMap::new(); m.insert("name", "Keyboard"); m.insert("price", 100); m }, { let mut m = std::collections::HashMap::new(); m.insert("name", "Mouse"); m.insert("price", 50); m }, { let mut m = std::collections::HashMap::new(); m.insert("name", "Headphones"); m.insert("price", 200); m }];
    let expensive = { let mut tmp1 = Vec::new();for &p in &products { let tmp2 = p; let tmp3 = -p.price; tmp1.push((tmp3, tmp2)); } tmp1.sort_by(|a,b| a.0.partial_cmp(&b.0).unwrap()); let mut tmp4 = Vec::new(); for p in tmp1 { tmp4.push(p.1); } let tmp4 = tmp4[1 as usize..(1 + 3) as usize].to_vec(); tmp4 };
    println!("{:?}", "--- Top products (excluding most expensive) ---");
    for item in expensive {
        println!("{:?} {:?} {:?}", item.name, "costs $", item.price);
    }
}
