// Generated by Mochi compiler v0.10.26 on 2025-07-15T07:22:50Z
#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Store_sale {
    item: i32,
    price: i32,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Catalog_sale {
    item: i32,
    price: i32,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Web_sale {
    item: i32,
    price: i32,
}

fn sum<T>(v: &[T]) -> T where T: std::iter::Sum<T> + Copy {
    v.iter().copied().sum()
}

fn _union_all<T: Clone>(mut a: Vec<T>, b: Vec<T>) -> Vec<T> {
    a.extend(b);
    a
}

fn _json<T: std::fmt::Debug>(value: &T) {
    println!("{:?}", value);
}

fn main() {
    let store_sales = vec![Store_sale { item: 1, price: 10 }, Store_sale { item: 1, price: 20 }];
    let catalog_sales = vec![Catalog_sale { item: 1, price: 15 }];
    let web_sales = vec![Web_sale { item: 1, price: 15 }];
    let all_sales = _union_all(_union_all(store_sales, catalog_sales), web_sales);
    let result = sum(&{ let mut tmp1 = Vec::new();for &s in &all_sales { tmp1.push(s.price); } tmp1 });
    _json(&result);
    assert!(result == 60 as f64);
}
