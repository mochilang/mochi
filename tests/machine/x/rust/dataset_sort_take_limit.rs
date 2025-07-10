#[derive(Default, Debug, Clone, PartialEq)]
struct Product {
    name: &'static str,
    price: i32,
}

fn main() {
    let products = vec![Product { name: "Laptop", price: 1500 }, Product { name: "Smartphone", price: 900 }, Product { name: "Tablet", price: 600 }, Product { name: "Monitor", price: 300 }, Product { name: "Keyboard", price: 100 }, Product { name: "Mouse", price: 50 }, Product { name: "Headphones", price: 200 }];
    let expensive = { let mut tmp1 = Vec::new();for p in &products { let tmp2 = p.clone(); let tmp3 = -p.price; tmp1.push((tmp3, tmp2)); } tmp1.sort_by(|a,b| a.0.partial_cmp(&b.0).unwrap()); let mut tmp4 = Vec::new(); for p in tmp1 { tmp4.push(p.1); } let tmp4 = tmp4[1 as usize..(1 + 3) as usize].to_vec(); tmp4 };
    println!("{}", "--- Top products (excluding most expensive) ---");
    for item in expensive {
        println!("{:?} {} {:?}", item.name, "costs $", item.price);
    }
}
