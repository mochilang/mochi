fn main() {
    let nums = [3, 1, 4];
    let min = nums.iter().min().unwrap();
    let max = nums.iter().max().unwrap();
    println!("{}", min);
    println!("{}", max);
}
