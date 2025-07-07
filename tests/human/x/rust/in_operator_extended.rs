use std::collections::HashMap;
fn main() {
    let xs = vec![1, 2, 3];
    let ys: Vec<i32> = xs.iter().cloned().filter(|x| x % 2 == 1).collect();
    println!("{}", ys.contains(&1));
    println!("{}", ys.contains(&2));

    let mut m = HashMap::new();
    m.insert("a", 1);
    println!("{}", m.contains_key("a"));
    println!("{}", m.contains_key("b"));

    let s = "hello";
    println!("{}", s.contains("ell"));
    println!("{}", s.contains("foo"));
}
