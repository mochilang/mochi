fn main() {
    let mut data = { let mut m = std::collections::HashMap::new(); m.insert("outer", { let mut m = std::collections::HashMap::new(); m.insert("inner", 1); m }); m };
    data["outer"]["inner"] = 2;
    println!("{:?}", data["outer"]["inner"]);
}
