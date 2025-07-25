// Generated by Mochi compiler v0.10.26 on 2025-07-15T07:22:55Z
#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
struct Store_sale {
    cust: &'static str,
    price: f64,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Catalog_sale {
    cust: &'static str,
}

fn sum<T>(v: &[T]) -> T where T: std::iter::Sum<T> + Copy {
    v.iter().copied().sum()
}

fn _json<T: std::fmt::Debug>(value: &T) {
    println!("{:?}", value);
}

fn main() {
    let store_sales = vec![Store_sale { cust: "A", price: 5.0 }, Store_sale { cust: "B", price: 30.0 }, Store_sale { cust: "C", price: 57.0 }];
    let catalog_sales = vec![Catalog_sale { cust: "A" }];
    let web_sales = vec![];
    let store_customers = { let mut tmp1 = Vec::new();for s in &store_sales { tmp1.push(s.cust); } tmp1 };
    let catalog_customers = { let mut tmp2 = Vec::new();for s in &catalog_sales { tmp2.push(s.cust); } tmp2 };
    let web_customers = { let mut tmp3 = Vec::new();for &s in &web_sales { tmp3.push(s.cust); } tmp3 };
    let store_only = { let mut tmp6 = Vec::new();for &c in &store_customers { if !({ let mut tmp4 = Vec::new();for &x in &catalog_customers { if !(x == c) { continue; } tmp4.push(x); } tmp4 }.len() as i32 == 0 && { let mut tmp5 = Vec::new();for &x in &web_customers { if !(x == c) { continue; } tmp5.push(x); } tmp5 }.len() as i32 == 0) { continue; } tmp6.push(c); } tmp6 };
    let result = sum(&{ let mut tmp8 = Vec::new();for s in &store_sales { if !({ let mut tmp7 = Vec::new();for &x in &store_only { if !(x == s.cust) { continue; } tmp7.push(x); } tmp7 }.len() as i32 > 0) { continue; } tmp8.push(s.price); } tmp8 });
    _json(&result);
    assert!(result == 87.0);
}
