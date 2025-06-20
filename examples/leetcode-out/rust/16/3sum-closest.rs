fn threeSumClosest(nums: Vec<i32>, target: i32) -> i32 {
    let mut sorted = {
    let mut _res = Vec::new();
    for n in nums {
        _res.push(n);
    }
    _res
};
    let mut n = sorted.len() as i32;
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

fn main() {
}

