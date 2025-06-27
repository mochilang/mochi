fn fourSum(nums: Vec<i64>, target: i64) -> Vec<Vec<i64>> {
    let mut sorted = {
        let mut _pairs = Vec::new();
        for n in nums {
            _pairs.push((n, n));
        }
        _pairs.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());
        let mut _res = Vec::new();
        for p in _pairs {
            _res.push(p.1);
        }
        _res
    };
    let mut n = sorted.len() as i64;
    let mut result: Vec<Vec<i64>> = vec![];
    for i in 0..n {
        if i > 0 && sorted[i as usize] == sorted[(i - 1) as usize] {
            continue;
        }
        for j in i + 1..n {
            if j > i + 1 && sorted[j as usize] == sorted[(j - 1) as usize] {
                continue;
            }
            let mut left = j + 1;
            let mut right = n - 1;
            while left < right {
                let mut sum = sorted[i as usize]
                    + sorted[j as usize]
                    + sorted[left as usize]
                    + sorted[right as usize];
                if sum == target {
                    result = {
                        let a = &result;
                        let b = &vec![vec![
                            sorted[i as usize],
                            sorted[j as usize],
                            sorted[left as usize],
                            sorted[right as usize],
                        ]];
                        let mut res = Vec::with_capacity(a.len() + b.len());
                        res.extend_from_slice(a);
                        res.extend_from_slice(b);
                        res
                    };
                    left = left + 1;
                    right = right - 1;
                    while left < right && sorted[left as usize] == sorted[(left - 1) as usize] {
                        left = left + 1;
                    }
                    while left < right && sorted[right as usize] == sorted[(right + 1) as usize] {
                        right = right - 1;
                    }
                } else if sum < target {
                    left = left + 1;
                } else {
                    right = right - 1;
                }
            }
        }
    }
    return result;
}

fn test_example_1() {
    expect(
        fourSum(vec![1, 0, -1, 0, -2, 2], 0)
            == vec![vec![-2, -1, 1, 2], vec![-2, 0, 0, 2], vec![-1, 0, 0, 1]],
    );
}

fn test_example_2() {
    expect(fourSum(vec![2, 2, 2, 2, 2], 8) == vec![vec![2, 2, 2, 2]]);
}

fn main() {
    test_example_1();
    test_example_2();
}

fn expect(cond: bool) {
    if !cond {
        panic!("expect failed");
    }
}
