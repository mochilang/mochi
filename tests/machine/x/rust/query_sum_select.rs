fn main() {
    let nums = vec![1, 2, 3];
    let result = { let mut tmp1 = 0;for &n in &nums { if !(n > 1) { continue; } tmp1 += n; } tmp1 };
    println!("{:?}", result);
}
