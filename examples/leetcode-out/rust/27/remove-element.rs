fn removeElement(nums: Vec<i64>, val: i64) -> i64 {
    let mut k = 0;
    let mut i = 0;
    while i < nums.len() as i64 {
        if nums[i as usize] != val {
            nums[k as usize] = nums[i as usize];
            k = k + 1;
        }
        i = i + 1;
    }
    return k;
}

fn test_example_1() {
    let mut nums = vec![3, 2, 2, 3];
    let mut k = removeElement(nums, 3);
    expect(k == 2);
    expect(nums[(0) as usize..k as usize].to_vec() == vec![2, 2]);
}

fn test_example_2() {
    let mut nums = vec![0, 1, 2, 2, 3, 0, 4, 2];
    let mut k = removeElement(nums, 2);
    expect(k == 5);
    expect(nums[(0) as usize..k as usize].to_vec() == vec![0, 1, 3, 0, 4]);
}

fn test_no_removal() {
    let mut nums = vec![1, 2, 3];
    let mut k = removeElement(nums, 4);
    expect(k == 3);
    expect(nums[(0) as usize..k as usize].to_vec() == vec![1, 2, 3]);
}

fn main() {
    test_example_1();
    test_example_2();
    test_no_removal();
}

fn expect(cond: bool) {
    if !cond {
        panic!("expect failed");
    }
}
