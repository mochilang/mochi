fn removeDuplicates(nums: Vec<i32>) -> i32 {
    if nums.len() as i32 == 0 {
        return 0;
    }
    let mut count = 1;
    let mut prev = nums[(0) as usize];
    let mut i = 1;
    while i < nums.len() as i32 {
        let mut cur = nums[i as usize];
        if cur != prev {
            count = count + 1;
            prev = cur;
        }
        i = i + 1;
    }
    return count;
}

fn main() {
}

