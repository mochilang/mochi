fn removeNthFromEnd(nums: Vec<i64>, n: i64) -> Vec<i64> {
    let mut idx = nums.len() as i64 - n;
    let mut result = vec![];
    let mut i = 0;
    while i < nums.len() as i64 {
        if i != idx {
            result = {
                let a = &result;
                let b = &vec![nums[i as usize]];
                let mut res = Vec::with_capacity(a.len() + b.len());
                res.extend_from_slice(a);
                res.extend_from_slice(b);
                res
            };
        }
        i = i + 1;
    }
    return result;
}

fn test_example_1() {
    expect(removeNthFromEnd(vec![1, 2, 3, 4, 5], 2) == vec![1, 2, 3, 5]);
}

fn test_example_2() {
    expect(removeNthFromEnd(vec![1], 1) == vec![]);
}

fn test_example_3() {
    expect(removeNthFromEnd(vec![1, 2], 1) == vec![1]);
}

fn test_remove_first() {
    expect(removeNthFromEnd(vec![7, 8, 9], 3) == vec![8, 9]);
}

fn test_remove_last() {
    expect(removeNthFromEnd(vec![7, 8, 9], 1) == vec![7, 8]);
}

fn main() {
    test_example_1();
    test_example_2();
    test_example_3();
    test_remove_first();
    test_remove_last();
}

fn expect(cond: bool) {
    if !cond {
        panic!("expect failed");
    }
}
