// Generated by Mochi compiler v0.10.28 on 1970-01-01T00:00:00Z
fn main() {
    let nums = vec![1, 2, 3];
    let result = { let mut tmp1 = 0;for &n in &nums { if !(n > 1) { continue; } tmp1 += n; } tmp1 };
    println!("{}", vec![format!("{}", result)].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
}
