fn swapPairs(nums: Vec<i64>) -> Vec<i64> {
    let mut i = 0;
    let mut result = vec![];
    while i < nums.len() as i64 {
        if i + 1 < nums.len() as i64 {
            result = {
                let a = &result;
                let b = &vec![nums[(i + 1) as usize], nums[i as usize]];
                let mut res = Vec::with_capacity(a.len() + b.len());
                res.extend_from_slice(a);
                res.extend_from_slice(b);
                res
            };
        } else {
            result = {
                let a = &result;
                let b = &vec![nums[i as usize]];
                let mut res = Vec::with_capacity(a.len() + b.len());
                res.extend_from_slice(a);
                res.extend_from_slice(b);
                res
            };
        }
        i = i + 2;
    }
    return result;
}

fn test_example_1() {
    expect(swapPairs(vec![1, 2, 3, 4]) == vec![2, 1, 4, 3]);
}

fn test_example_2() {
    expect(swapPairs(vec![]) == vec![]);
}

fn test_example_3() {
    expect(swapPairs(vec![1]) == vec![1]);
}

fn test_odd_length() {
    expect(swapPairs(vec![1, 2, 3]) == vec![2, 1, 3]);
}

fn main() {
    test_example_1();
    test_example_2();
    test_example_3();
    test_odd_length();
}

fn expect(cond: bool) {
    if !cond {
        panic!("expect failed");
    }
}
