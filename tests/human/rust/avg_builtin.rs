fn avg(nums: &[i32]) -> f64 {
    let sum: i32 = nums.iter().sum();
    sum as f64 / nums.len() as f64
}

fn main() {
    println!("{}", avg(&[1, 2, 3]));
}
