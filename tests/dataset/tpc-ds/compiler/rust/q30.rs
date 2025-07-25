// Generated by Mochi compiler v0.10.26 on 2025-07-15T07:22:43Z
#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
struct Web_return {
    wr_returning_customer_sk: i32,
    wr_returned_date_sk: i32,
    wr_return_amt: f64,
    wr_returning_addr_sk: i32,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Date_dim {
    d_date_sk: i32,
    d_year: i32,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Customer_addres {
    ca_address_sk: i32,
    ca_state: &'static str,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Customer {
    c_customer_sk: i32,
    c_customer_id: &'static str,
    c_first_name: &'static str,
    c_last_name: &'static str,
    c_current_addr_sk: i32,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Key {
    cust: i32,
    state: &'static str,
}

#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
struct Item {
    wr: Web_return,
    d: Date_dim,
    ca: Customer_addres,
}

#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
struct Group {
    key: Key,
    items: Vec<Item>,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Result {
    ctr_customer_sk: i32,
    ctr_state: &'static str,
    ctr_total_return: i32,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Group1 {
    key: &'static str,
    items: Vec<Result>,
}

#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
struct Result2 {
    state: &'static str,
    avg_return: f64,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Result3 {
    c_customer_id: &'static str,
    c_first_name: &'static str,
    c_last_name: &'static str,
    ctr_total_return: i32,
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
    let web_returns = vec![Web_return { wr_returning_customer_sk: 1, wr_returned_date_sk: 1, wr_return_amt: 100.0, wr_returning_addr_sk: 1 }, Web_return { wr_returning_customer_sk: 2, wr_returned_date_sk: 1, wr_return_amt: 30.0, wr_returning_addr_sk: 2 }, Web_return { wr_returning_customer_sk: 1, wr_returned_date_sk: 1, wr_return_amt: 50.0, wr_returning_addr_sk: 1 }];
    let date_dim = vec![Date_dim { d_date_sk: 1, d_year: 2000 }];
    let customer_address = vec![Customer_addres { ca_address_sk: 1, ca_state: "CA" }, Customer_addres { ca_address_sk: 2, ca_state: "CA" }];
    let customer = vec![Customer { c_customer_sk: 1, c_customer_id: "C1", c_first_name: "John", c_last_name: "Doe", c_current_addr_sk: 1 }, Customer { c_customer_sk: 2, c_customer_id: "C2", c_first_name: "Jane", c_last_name: "Smith", c_current_addr_sk: 2 }];
    let customer_total_return = { let mut tmp1 = std::collections::HashMap::new();for wr in &web_returns { for d in &date_dim { if !(wr.wr_returned_date_sk == d.d_date_sk) { continue; } for ca in &customer_address { if !(wr.wr_returning_addr_sk == ca.ca_address_sk) { continue; } if !(d.d_year == 2000 && ca.ca_state == "CA") { continue; } let key = Key { cust: wr.wr_returning_customer_sk, state: ca.ca_state }; tmp1.entry(key).or_insert_with(Vec::new).push(Item {wr: wr.clone(), d: d.clone(), ca: ca.clone() }); } } } let mut tmp2 = Vec::<Group>::new(); for (k,v) in tmp1 { tmp2.push(Group { key: k, items: v }); } tmp2.sort_by(|a,b| a.key.partial_cmp(&b.key).unwrap()); let mut result = Vec::new(); for g in tmp2 { result.push(Result { ctr_customer_sk: g.key.cust, ctr_state: g.key.state, ctr_total_return: sum(&{ let mut tmp3 = Vec::new();for x in &g.clone().items { tmp3.push(x.wr_return_amt); } tmp3 }) }); } result };
    let avg_by_state = { let mut tmp4 = std::collections::HashMap::new();for ctr in &customer_total_return { let key = ctr.ctr_state; tmp4.entry(key).or_insert_with(Vec::new).push(ctr.clone()); } let mut tmp5 = Vec::<Group1>::new(); for (k,v) in tmp4 { tmp5.push(Group1 { key: k, items: v }); } tmp5.sort_by(|a,b| a.key.partial_cmp(&b.key).unwrap()); let mut result = Vec::new(); for g in tmp5 { result.push(Result2 { state: g.key, avg_return: avg(&{ let mut tmp6 = Vec::new();for x in &g.clone().items { tmp6.push(x.ctr_total_return); } tmp6 }) }); } result };
    let result = { let mut tmp7 = Vec::new();for ctr in &customer_total_return { for avg in &avg_by_state { if !(ctr.ctr_state == avg.state) { continue; } for c in &customer { if !(ctr.ctr_customer_sk == c.c_customer_sk) { continue; } if !((ctr.ctr_total_return as f64) > avg.avg_return * 1.2) { continue; } tmp7.push(Result3 { c_customer_id: c.c_customer_id, c_first_name: c.c_first_name, c_last_name: c.c_last_name, ctr_total_return: ctr.ctr_total_return }); } } } tmp7 };
    _json(&result);
    assert!(result == vec![Result3 { c_customer_id: "C1", c_first_name: "John", c_last_name: "Doe", ctr_total_return: 150.0 }]);
}
