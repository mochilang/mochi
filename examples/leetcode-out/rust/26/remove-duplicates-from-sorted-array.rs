fn removeDuplicates(nums: Vec<i64>) -> i64 {
    if nums.len() as i64 == 0 {
        return 0;
    }
    let mut count = 1;
    let mut prev = nums[(0) as usize];
    let mut i = 1;
    while i < nums.len() as i64 {
        let mut cur = nums[i as usize];
        if cur != prev {
            count = count + 1;
            prev = cur;
        }
        i = i + 1;
    }
    return count;
}

fn test_example_1() {
    expect(removeDuplicates(vec![1, 1, 2]) == 2);
}

fn test_example_2() {
    expect(removeDuplicates(vec![0, 0, 1, 1, 1, 2, 2, 3, 3, 4]) == 5);
}

fn test_empty() {
    expect(removeDuplicates(vec![]) == 0);
}

fn main() {
    test_example_1();
    test_example_2();
    test_empty();
}

fn expect(cond: bool) {
    if !cond {
        panic!("expect failed");
    }
}
