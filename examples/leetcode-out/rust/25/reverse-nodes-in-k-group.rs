fn reverseKGroup(nums: Vec<i64>, k: i64) -> Vec<i64> {
    let mut n = nums.len() as i64;
    if k <= 1 {
        return nums;
    }
    let mut result = vec![];
    let mut i = 0;
    while i < n {
        let mut end = i + k;
        if end <= n {
            let mut j = end - 1;
            while j >= i {
                result = {
                    let a = &result;
                    let b = &vec![nums[j as usize]];
                    let mut res = Vec::with_capacity(a.len() + b.len());
                    res.extend_from_slice(a);
                    res.extend_from_slice(b);
                    res
                };
                j = j - 1;
            }
        } else {
            let mut j = i;
            while j < n {
                result = {
                    let a = &result;
                    let b = &vec![nums[j as usize]];
                    let mut res = Vec::with_capacity(a.len() + b.len());
                    res.extend_from_slice(a);
                    res.extend_from_slice(b);
                    res
                };
                j = j + 1;
            }
        }
        i = i + k;
    }
    return result;
}

fn test_example_1() {
    expect(reverseKGroup(vec![1, 2, 3, 4, 5], 2) == vec![2, 1, 4, 3, 5]);
}

fn test_example_2() {
    expect(reverseKGroup(vec![1, 2, 3, 4, 5], 3) == vec![3, 2, 1, 4, 5]);
}

fn test_k_equals_list_length() {
    expect(reverseKGroup(vec![1, 2, 3, 4], 4) == vec![4, 3, 2, 1]);
}

fn test_k_greater_than_length() {
    expect(reverseKGroup(vec![1, 2, 3], 5) == vec![1, 2, 3]);
}

fn test_k_is_one() {
    expect(reverseKGroup(vec![1, 2, 3], 1) == vec![1, 2, 3]);
}

fn main() {
    test_example_1();
    test_example_2();
    test_k_equals_list_length();
    test_k_greater_than_length();
    test_k_is_one();
}

fn expect(cond: bool) {
    if !cond {
        panic!("expect failed");
    }
}
