fn main() {
    let nums = [1.0, 2.0, 3.0];
    let sum: f64 = nums.iter().sum();
    println!("{}", sum / nums.len() as f64);
}
