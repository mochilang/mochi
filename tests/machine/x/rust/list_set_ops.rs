// Generated by Mochi compiler v0.10.28 on 1970-01-01T00:00:00Z
fn main() {
    { for (i, it) in { let mut set: std::collections::HashSet<_> = vec![1, 2].into_iter().collect(); set.extend(vec![2, 3].into_iter()); let mut v: Vec<_> = set.into_iter().collect(); v.sort(); v }.iter().enumerate() { if i > 0 { print!(" "); } print!("{}", it); } println!(); };
    { for (i, it) in { let set: std::collections::HashSet<_> = vec![2].into_iter().collect(); vec![1, 2, 3].into_iter().filter(|x| !set.contains(x)).collect::<Vec<_>>() }.iter().enumerate() { if i > 0 { print!(" "); } print!("{}", it); } println!(); };
    { for (i, it) in { let set: std::collections::HashSet<_> = vec![2, 4].into_iter().collect(); vec![1, 2, 3].into_iter().filter(|x| set.contains(x)).collect::<Vec<_>>() }.iter().enumerate() { if i > 0 { print!(" "); } print!("{}", it); } println!(); };
    println!("{}", vec![format!("{}", { let mut tmp = vec![1, 2]; tmp.extend(vec![2, 3]); tmp }.len() as i32)].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
}
