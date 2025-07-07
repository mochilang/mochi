fn main() {
    let nums = vec![1, 2, 3];
    let sum: i32 = nums.iter().filter(|&&n| n > 1).sum();
    println!("{}", sum);
}
