fn main() {
    fn twoSum(nums: Vec<i32>, target: i32) -> Vec<i32> {
        let n = nums.len();
        for i in (0 as i32)..(n as i32) {
            for j in (i + 1 as i32)..(n as i32) {
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
