fn sum(v: &[i32]) -> i32 {
    v.iter().sum()
}

fn main() {
    let nums = vec![1, 2, 3];
    let result = { let mut tmp1 = Vec::new();for &n in &nums { if !(n > 1) { continue; } tmp1.push(sum(&n)); } tmp1 };
    println!("{:?}", result);
}
