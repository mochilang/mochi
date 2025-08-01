// Generated by Mochi compiler v0.10.25 on 2025-07-13T17:34:44Z
#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Part {
    p_partkey: i32,
    p_type: &'static str,
}

#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
struct Lineitem {
    l_partkey: i32,
    l_extendedprice: f64,
    l_discount: f64,
    l_shipdate: &'static str,
}

#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
struct Result {
    is_promo: bool,
    revenue: f64,
}

fn sum<T>(v: &[T]) -> T where T: std::iter::Sum<T> + Copy {
    v.iter().copied().sum()
}

fn _json<T: std::fmt::Debug>(value: &T) {
    println!("{:?}", value);
}

fn main() {
    let part = vec![Part { p_partkey: 1, p_type: "PROMO LUXURY" }, Part { p_partkey: 2, p_type: "STANDARD BRASS" }];
    let lineitem = vec![Lineitem { l_partkey: 1, l_extendedprice: 1000.0, l_discount: 0.1, l_shipdate: "1995-09-05" }, Lineitem { l_partkey: 2, l_extendedprice: 800.0, l_discount: 0.0, l_shipdate: "1995-09-20" }, Lineitem { l_partkey: 1, l_extendedprice: 500.0, l_discount: 0.2, l_shipdate: "1995-10-02" }];
    let start_date = "1995-09-01";
    let end_date = "1995-10-01";
    let filtered = { let mut tmp1 = Vec::new();for l in &lineitem { for p in &part { if !(p.p_partkey == l.l_partkey) { continue; } if !(l.l_shipdate >= start_date && l.l_shipdate < end_date) { continue; } tmp1.push(Result { is_promo: p.p_type.contains("PROMO"), revenue: l.l_extendedprice * ((1 as f64) - l.l_discount) }); } } tmp1 };
    let promo_sum = sum(&{ let mut tmp2 = Vec::new();for x in &filtered { if !(x.is_promo) { continue; } tmp2.push(x.revenue); } tmp2 });
    let total_sum = sum(&{ let mut tmp3 = Vec::new();for x in &filtered { tmp3.push(x.revenue); } tmp3 });
    let result = 100.0 * promo_sum / total_sum;
    _json(&result);
    let promo = 1000.0 * 0.9;
    let total = (((((900 as f64) as f64) as f64) as f64) as f64) + 800.0;
    let expected = 100.0 * promo / total;
    assert!(result == expected);
}
