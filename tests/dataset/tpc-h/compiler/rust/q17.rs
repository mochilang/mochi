// Generated by Mochi compiler v0.10.25 on 2025-07-13T18:21:15Z
#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Part {
    p_partkey: i32,
    p_brand: &'static str,
    p_container: &'static str,
}

#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
struct Lineitem {
    l_partkey: i32,
    l_quantity: i32,
    l_extendedprice: f64,
}

fn avg<T>(v: &[T]) -> f64 where T: Into<f64> + Copy {
    let sum: f64 = v.iter().map(|&x| x.into()).sum();
    sum / v.len() as f64
}

fn sum<T>(v: &[T]) -> T where T: std::iter::Sum<T> + Copy {
    v.iter().copied().sum()
}

fn _json<T: std::fmt::Debug>(value: &T) {
    println!("{:?}", value);
}

fn main() {
    let part = vec![Part { p_partkey: 1, p_brand: "Brand#23", p_container: "MED BOX" }, Part { p_partkey: 2, p_brand: "Brand#77", p_container: "LG JAR" }];
    let lineitem = vec![Lineitem { l_partkey: 1, l_quantity: 1, l_extendedprice: 100.0 }, Lineitem { l_partkey: 1, l_quantity: 10, l_extendedprice: 1000.0 }, Lineitem { l_partkey: 1, l_quantity: 20, l_extendedprice: 2000.0 }, Lineitem { l_partkey: 2, l_quantity: 5, l_extendedprice: 500.0 }];
    let brand = "Brand#23";
    let container = "MED BOX";
    let filtered = { let mut tmp2 = Vec::new();for l in &lineitem { for p in &part { if !(p.p_partkey == l.l_partkey) { continue; } if !(((p.p_brand == brand) && (p.p_container == container) && ((l.l_quantity as f64) < (0.2 * avg(&{ let mut tmp1 = Vec::new();for x in &lineitem { if !(x.l_partkey == p.p_partkey) { continue; } tmp1.push(x.l_quantity); } tmp1 }))))) { continue; } tmp2.push(l.l_extendedprice); } } tmp2 };
    let result = sum(&filtered) / 7.0;
    _json(&result);
    let expected = 100.0 / 7.0;
    assert!(result == expected);
}
