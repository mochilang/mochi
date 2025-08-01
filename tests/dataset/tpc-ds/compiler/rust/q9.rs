// Generated by Mochi compiler v0.10.26 on 2025-07-15T07:22:38Z
#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
struct Store_sale {
    ss_quantity: i32,
    ss_ext_discount_amt: f64,
    ss_net_paid: f64,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Reason {
    r_reason_sk: i32,
}

#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
struct Result {
    bucket1: f64,
    bucket2: f64,
    bucket3: f64,
    bucket4: f64,
    bucket5: f64,
}

fn avg<T>(v: &[T]) -> f64 where T: Into<f64> + Copy {
    let sum: f64 = v.iter().map(|&x| x.into()).sum();
    sum / v.len() as f64
}

fn _json<T: std::fmt::Debug>(value: &T) {
    println!("{:?}", value);
}

fn main() {
    let store_sales = vec![Store_sale { ss_quantity: 5, ss_ext_discount_amt: 5.0, ss_net_paid: 7.0 }, Store_sale { ss_quantity: 30, ss_ext_discount_amt: 10.0, ss_net_paid: 15.0 }, Store_sale { ss_quantity: 50, ss_ext_discount_amt: 20.0, ss_net_paid: 30.0 }, Store_sale { ss_quantity: 70, ss_ext_discount_amt: 25.0, ss_net_paid: 35.0 }, Store_sale { ss_quantity: 90, ss_ext_discount_amt: 40.0, ss_net_paid: 50.0 }];
    let reason = vec![Reason { r_reason_sk: 1 }];
    let bucket1 = if { let mut tmp1 = Vec::new();for s in &store_sales { if !(s.ss_quantity >= 1 && s.ss_quantity <= 20) { continue; } tmp1.push(s.clone()); } tmp1 }.len() as i32 > 10 { avg(&{ let mut tmp2 = Vec::new();for s in &store_sales { if !(s.ss_quantity >= 1 && s.ss_quantity <= 20) { continue; } tmp2.push(s.ss_ext_discount_amt); } tmp2 }) } else { avg(&{ let mut tmp3 = Vec::new();for s in &store_sales { if !(s.ss_quantity >= 1 && s.ss_quantity <= 20) { continue; } tmp3.push(s.ss_net_paid); } tmp3 }) };
    let bucket2 = if { let mut tmp4 = Vec::new();for s in &store_sales { if !(s.ss_quantity >= 21 && s.ss_quantity <= 40) { continue; } tmp4.push(s.clone()); } tmp4 }.len() as i32 > 20 { avg(&{ let mut tmp5 = Vec::new();for s in &store_sales { if !(s.ss_quantity >= 21 && s.ss_quantity <= 40) { continue; } tmp5.push(s.ss_ext_discount_amt); } tmp5 }) } else { avg(&{ let mut tmp6 = Vec::new();for s in &store_sales { if !(s.ss_quantity >= 21 && s.ss_quantity <= 40) { continue; } tmp6.push(s.ss_net_paid); } tmp6 }) };
    let bucket3 = if { let mut tmp7 = Vec::new();for s in &store_sales { if !(s.ss_quantity >= 41 && s.ss_quantity <= 60) { continue; } tmp7.push(s.clone()); } tmp7 }.len() as i32 > 30 { avg(&{ let mut tmp8 = Vec::new();for s in &store_sales { if !(s.ss_quantity >= 41 && s.ss_quantity <= 60) { continue; } tmp8.push(s.ss_ext_discount_amt); } tmp8 }) } else { avg(&{ let mut tmp9 = Vec::new();for s in &store_sales { if !(s.ss_quantity >= 41 && s.ss_quantity <= 60) { continue; } tmp9.push(s.ss_net_paid); } tmp9 }) };
    let bucket4 = if { let mut tmp10 = Vec::new();for s in &store_sales { if !(s.ss_quantity >= 61 && s.ss_quantity <= 80) { continue; } tmp10.push(s.clone()); } tmp10 }.len() as i32 > 40 { avg(&{ let mut tmp11 = Vec::new();for s in &store_sales { if !(s.ss_quantity >= 61 && s.ss_quantity <= 80) { continue; } tmp11.push(s.ss_ext_discount_amt); } tmp11 }) } else { avg(&{ let mut tmp12 = Vec::new();for s in &store_sales { if !(s.ss_quantity >= 61 && s.ss_quantity <= 80) { continue; } tmp12.push(s.ss_net_paid); } tmp12 }) };
    let bucket5 = if { let mut tmp13 = Vec::new();for s in &store_sales { if !(s.ss_quantity >= 81 && s.ss_quantity <= 100) { continue; } tmp13.push(s.clone()); } tmp13 }.len() as i32 > 50 { avg(&{ let mut tmp14 = Vec::new();for s in &store_sales { if !(s.ss_quantity >= 81 && s.ss_quantity <= 100) { continue; } tmp14.push(s.ss_ext_discount_amt); } tmp14 }) } else { avg(&{ let mut tmp15 = Vec::new();for s in &store_sales { if !(s.ss_quantity >= 81 && s.ss_quantity <= 100) { continue; } tmp15.push(s.ss_net_paid); } tmp15 }) };
    let result = { let mut tmp16 = Vec::new();for r in &reason { if !(r.r_reason_sk == 1) { continue; } tmp16.push(Result { bucket1: bucket1, bucket2: bucket2, bucket3: bucket3, bucket4: bucket4, bucket5: bucket5 }); } tmp16 };
    _json(&result);
    assert!(result == vec![Result { bucket1: 7.0, bucket2: 15.0, bucket3: 30.0, bucket4: 35.0, bucket5: 50.0 }]);
}
