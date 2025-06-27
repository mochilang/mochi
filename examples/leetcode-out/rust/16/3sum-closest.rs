fn threeSumClosest(nums: Vec<i64>, target: i64) -> i64 {
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
    let mut best = sorted[(0) as usize] + sorted[(1) as usize] + sorted[(2) as usize];
    for i in 0..n {
        let mut left = i + 1;
        let mut right = n - 1;
        while left < right {
            let mut sum = sorted[i as usize] + sorted[left as usize] + sorted[right as usize];
            if sum == target {
                return target;
            }
            let mut diff = 0;
            if sum > target {
                diff = sum - target;
            } else {
                diff = target - sum;
            }
            let mut bestDiff = 0;
            if best > target {
                bestDiff = best - target;
            } else {
                bestDiff = target - best;
            }
            if diff < bestDiff {
                best = sum;
            }
            if sum < target {
                left = left + 1;
            } else {
                right = right - 1;
            }
        }
    }
    return best;
}

fn test_example_1() {
    expect(threeSumClosest(vec![-1, 2, 1, -4], 1) == 2);
}

fn test_example_2() {
    expect(threeSumClosest(vec![0, 0, 0], 1) == 0);
}

fn test_additional() {
    expect(threeSumClosest(vec![1, 1, 1, 0], -100) == 2);
}

fn main() {
    test_example_1();
    test_example_2();
    test_additional();
}

fn expect(cond: bool) {
    if !cond {
        panic!("expect failed");
    }
}
