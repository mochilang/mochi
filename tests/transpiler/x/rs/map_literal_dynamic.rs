// Generated by Mochi transpiler v0.10.33 on 2025-07-21 18:06 +0700
use std::collections::HashMap;
fn main() {
    let mut x: i64 = 3;
    let mut y: i64 = 4;
    let mut m = HashMap::from([("a", x), ("b", y)]);
    println!("{}", format!("{} {}", m[&"a"], m[&"b"]).trim_end());
}
