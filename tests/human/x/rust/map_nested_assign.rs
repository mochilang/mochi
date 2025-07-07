use std::collections::HashMap;

fn main() {
    let mut inner = HashMap::new();
    inner.insert("inner", 1);
    let mut data = HashMap::new();
    data.insert("outer", inner);
    if let Some(map) = data.get_mut("outer") {
        map.insert("inner", 2);
    }
    println!("{}", data["outer"]["inner"]);
}
