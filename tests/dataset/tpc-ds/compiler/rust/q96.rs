// Generated by Mochi compiler v0.10.26 on 2025-07-15T07:22:57Z
#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct StoreSale {
        ss_sold_time_sk: i32,
        ss_hdemo_sk: i32,
        ss_store_sk: i32,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct HouseholdDemographics {
        hd_demo_sk: i32,
        hd_dep_count: i32,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct TimeDim {
        t_time_sk: i32,
        t_hour: i32,
        t_minute: i32,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct Store {
        s_store_sk: i32,
        s_store_name: &'static str,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Store_sale {
    ss_sold_time_sk: i32,
    ss_hdemo_sk: i32,
    ss_store_sk: i32,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Household_demographic {
    hd_demo_sk: i32,
    hd_dep_count: i32,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Time_dim {
    t_time_sk: i32,
    t_hour: i32,
    t_minute: i32,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Store1 {
    s_store_sk: i32,
    s_store_name: &'static str,
}

fn _json<T: std::fmt::Debug>(value: &T) {
    println!("{:?}", value);
}

fn main() {
    let store_sales = vec![Store_sale { ss_sold_time_sk: 1, ss_hdemo_sk: 1, ss_store_sk: 1 }, Store_sale { ss_sold_time_sk: 1, ss_hdemo_sk: 1, ss_store_sk: 1 }, Store_sale { ss_sold_time_sk: 2, ss_hdemo_sk: 1, ss_store_sk: 1 }];
    let household_demographics = vec![Household_demographic { hd_demo_sk: 1, hd_dep_count: 3 }];
    let time_dim = vec![Time_dim { t_time_sk: 1, t_hour: 20, t_minute: 35 }, Time_dim { t_time_sk: 2, t_hour: 20, t_minute: 45 }];
    let store = vec![Store1 { s_store_sk: 1, s_store_name: "ese" }];
    let result = { let mut tmp1 = Vec::new();for ss in &store_sales { for hd in &household_demographics { if !(ss.ss_hdemo_sk == hd.hd_demo_sk) { continue; } for t in &time_dim { if !(ss.ss_sold_time_sk == t.t_time_sk) { continue; } for s in &store { if !(ss.ss_store_sk == s.s_store_sk) { continue; } if !(t.t_hour == 20 && t.t_minute >= 30 && hd.hd_dep_count == 3 && s.s_store_name == "ese") { continue; } tmp1.push(ss.clone()); } } } } tmp1 }.len() as i32;
    _json(&result);
    assert!(result == 3);
}
