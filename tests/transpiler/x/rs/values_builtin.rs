// Generated by Mochi transpiler v0.10.33 on 2025-07-21 18:06 +0700
use std::collections::HashMap;
fn main() {
    let m = HashMap::from([("a", 1), ("b", 2), ("c", 3)]);
    println!("{}", { let tmp = { let mut v = m.values().cloned().collect::<Vec<_>>(); v.sort(); v }; tmp.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(" ") });
}
