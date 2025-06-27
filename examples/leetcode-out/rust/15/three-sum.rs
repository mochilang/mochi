fn threeSum(nums: Vec<i64>) -> Vec<Vec<i64>> {
    let mut sorted = {
        let mut _pairs = Vec::new();
        for x in nums {
            _pairs.push((x, x));
        }
        _pairs.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());
        let mut _res = Vec::new();
        for p in _pairs {
            _res.push(p.1);
        }
        _res
    };
    let mut n = sorted.len() as i64;
    let mut res: Vec<Vec<i64>> = vec![];
    let mut i = 0;
    while i < n {
        if i > 0 && sorted[i as usize] == sorted[(i - 1) as usize] {
            i = i + 1;
            continue;
        }
        let mut left = i + 1;
        let mut right = n - 1;
        while left < right {
            let mut sum = sorted[i as usize] + sorted[left as usize] + sorted[right as usize];
            if sum == 0 {
                res = {
                    let a = &res;
                    let b = &vec![vec![
                        sorted[i as usize],
                        sorted[left as usize],
                        sorted[right as usize],
                    ]];
                    let mut res = Vec::with_capacity(a.len() + b.len());
                    res.extend_from_slice(a);
                    res.extend_from_slice(b);
                    res
                };
                left = left + 1;
                while left < right && sorted[left as usize] == sorted[(left - 1) as usize] {
                    left = left + 1;
                }
                right = right - 1;
                while left < right && sorted[right as usize] == sorted[(right + 1) as usize] {
                    right = right - 1;
                }
            } else if sum < 0 {
                left = left + 1;
            } else {
                right = right - 1;
            }
        }
        i = i + 1;
    }
    return res;
}

fn test_example_1() {
    expect(threeSum(vec![-1, 0, 1, 2, -1, -4]) == vec![vec![-1, -1, 2], vec![-1, 0, 1]]);
}

fn test_example_2() {
    expect(threeSum(vec![0, 1, 1]) == vec![]);
}

fn test_example_3() {
    expect(threeSum(vec![0, 0, 0]) == vec![vec![0, 0, 0]]);
}

fn main() {
    test_example_1();
    test_example_2();
    test_example_3();
}

fn expect(cond: bool) {
    if !cond {
        panic!("expect failed");
    }
}
