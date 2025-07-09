fn main() {
    let mut data = { let mut m = std::collections::HashMap::new(); m.insert("outer", { let mut m = std::collections::HashMap::new(); m.insert("inner", 1); m }); m };
    if let Some(tmp1) = data.get_mut("outer") {
        tmp1.insert("inner", 2);
    }
    println!("{:?}", data[&"outer"][&"inner"]);
}
