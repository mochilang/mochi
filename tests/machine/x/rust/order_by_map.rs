#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct Data {
    a: i32,
    b: i32,
}

fn main() {
    let data = vec![Data { a: 1, b: 2 }, Data { a: 1, b: 1 }, Data { a: 0, b: 5 }];
    let sorted = { let mut tmp1 = Vec::new();for x in &data { let tmp2 = x.clone(); let tmp3 = (x.a, x.b); tmp1.push((tmp3, tmp2)); } tmp1.sort_by(|a,b| a.0.partial_cmp(&b.0).unwrap()); let mut tmp4 = Vec::new(); for p in tmp1 { tmp4.push(p.1); } tmp4 };
    println!("{:?}", sorted);
}
