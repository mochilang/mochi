// Generated by Mochi compiler v0.10.26 on 2025-07-15T07:22:43Z
#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
struct Store_sale {
    ca_county: &'static str,
    d_qoy: i32,
    d_year: i32,
    ss_ext_sales_price: f64,
}

#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
struct Web_sale {
    ca_county: &'static str,
    d_qoy: i32,
    d_year: i32,
    ws_ext_sales_price: f64,
}

#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
struct Item {
    ca_county: &'static str,
    d_year: i32,
    web_q1_q2_increase: f64,
    store_q1_q2_increase: f64,
    web_q2_q3_increase: f64,
    store_q2_q3_increase: f64,
}

fn append<T: Clone>(mut v: Vec<T>, item: T) -> Vec<T> {
    v.push(item);
    v
}

fn sum<T>(v: &[T]) -> T where T: std::iter::Sum<T> + Copy {
    v.iter().copied().sum()
}

fn _json<T: std::fmt::Debug>(value: &T) {
    println!("{:?}", value);
}

fn main() {
    let store_sales = vec![Store_sale { ca_county: "A", d_qoy: 1, d_year: 2000, ss_ext_sales_price: 100.0 }, Store_sale { ca_county: "A", d_qoy: 2, d_year: 2000, ss_ext_sales_price: 120.0 }, Store_sale { ca_county: "A", d_qoy: 3, d_year: 2000, ss_ext_sales_price: 160.0 }, Store_sale { ca_county: "B", d_qoy: 1, d_year: 2000, ss_ext_sales_price: 80.0 }, Store_sale { ca_county: "B", d_qoy: 2, d_year: 2000, ss_ext_sales_price: 90.0 }, Store_sale { ca_county: "B", d_qoy: 3, d_year: 2000, ss_ext_sales_price: 100.0 }];
    let web_sales = vec![Web_sale { ca_county: "A", d_qoy: 1, d_year: 2000, ws_ext_sales_price: 100.0 }, Web_sale { ca_county: "A", d_qoy: 2, d_year: 2000, ws_ext_sales_price: 150.0 }, Web_sale { ca_county: "A", d_qoy: 3, d_year: 2000, ws_ext_sales_price: 250.0 }, Web_sale { ca_county: "B", d_qoy: 1, d_year: 2000, ws_ext_sales_price: 80.0 }, Web_sale { ca_county: "B", d_qoy: 2, d_year: 2000, ws_ext_sales_price: 90.0 }, Web_sale { ca_county: "B", d_qoy: 3, d_year: 2000, ws_ext_sales_price: 95.0 }];
    let counties = vec!["A", "B"];
    let mut result = vec![];
    for county in counties {
        let ss1 = sum(&{ let mut tmp1 = Vec::new();for s in &store_sales { if !(s.ca_county == county && s.d_qoy == 1) { continue; } tmp1.push(s.ss_ext_sales_price); } tmp1 });
        let ss2 = sum(&{ let mut tmp2 = Vec::new();for s in &store_sales { if !(s.ca_county == county && s.d_qoy == 2) { continue; } tmp2.push(s.ss_ext_sales_price); } tmp2 });
        let ss3 = sum(&{ let mut tmp3 = Vec::new();for s in &store_sales { if !(s.ca_county == county && s.d_qoy == 3) { continue; } tmp3.push(s.ss_ext_sales_price); } tmp3 });
        let ws1 = sum(&{ let mut tmp4 = Vec::new();for w in &web_sales { if !(w.ca_county == county && w.d_qoy == 1) { continue; } tmp4.push(w.ws_ext_sales_price); } tmp4 });
        let ws2 = sum(&{ let mut tmp5 = Vec::new();for w in &web_sales { if !(w.ca_county == county && w.d_qoy == 2) { continue; } tmp5.push(w.ws_ext_sales_price); } tmp5 });
        let ws3 = sum(&{ let mut tmp6 = Vec::new();for w in &web_sales { if !(w.ca_county == county && w.d_qoy == 3) { continue; } tmp6.push(w.ws_ext_sales_price); } tmp6 });
        let web_g1 = ws2 / ws1;
        let store_g1 = ss2 / ss1;
        let web_g2 = ws3 / ws2;
        let store_g2 = ss3 / ss2;
        if web_g1 > store_g1 && web_g2 > store_g2 {
            result = append(result, Item { ca_county: county, d_year: 2000, web_q1_q2_increase: web_g1, store_q1_q2_increase: store_g1, web_q2_q3_increase: web_g2, store_q2_q3_increase: store_g2 });
        }
    }
    _json(&result);
    assert!(result == vec![Item { ca_county: "A", d_year: 2000, web_q1_q2_increase: 1.5, store_q1_q2_increase: 1.2, web_q2_q3_increase: 1.6666666666666667, store_q2_q3_increase: 1.3333333333333333 }]);
}
