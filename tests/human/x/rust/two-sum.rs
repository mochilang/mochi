fn two_sum(nums: &[i32], target: i32) -> (i32, i32) {
    for i in 0..nums.len() {
        for j in (i+1)..nums.len() {
            if nums[i] + nums[j] == target {
                return (i as i32, j as i32);
            }
        }
    }
    (-1, -1)
}

fn main() {
    let result = two_sum(&[2, 7, 11, 15], 9);
    println!("{}", result.0);
    println!("{}", result.1);
}
