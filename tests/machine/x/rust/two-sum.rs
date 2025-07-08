fn main() {
    fn twoSum(nums: i32, target: i32) -> i32 {
        let n = nums.len();
        for i in 0..n {
            for j in i + 1..n {
                if nums[i as usize] + nums[j as usize] == target {
                    return vec![i, j];
                }
            }
        }
        return vec![-1, -1];
    }
    let result = twoSum(vec![2, 7, 11, 15], 9);
    println!("{:?}", result[0 as usize]);
    println!("{:?}", result[1 as usize]);
}
