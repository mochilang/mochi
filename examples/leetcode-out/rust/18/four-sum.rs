fn fourSum(nums: Vec<i32>, target: i32) -> Vec<Vec<i32>> {
    let mut sorted = {
    let mut _res = Vec::new();
    for n in nums {
        _res.push(n);
    }
    _res
};
    let mut n = sorted.len() as i32;
    let mut result: Vec<Vec<i32>> = vec![];
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
                let mut sum = sorted[i as usize] + sorted[j as usize] + sorted[left as usize] + sorted[right as usize];
                if sum == target {
                    result = _concat(&result, &vec![vec![sorted[i as usize], sorted[j as usize], sorted[left as usize], sorted[right as usize]]]);
                    left = left + 1;
                    right = right - 1;
                    while left < right && sorted[left as usize] == sorted[(left - 1) as usize] {
                        left = left + 1;
                    }
                    while left < right && sorted[right as usize] == sorted[(right + 1) as usize] {
                        right = right - 1;
                    }
                } else 
                if sum < target {
                    left = left + 1;
                } else {
                    right = right - 1;
                }
            }
        }
    }
    return result;
}

fn main() {
}

fn _concat<T: Clone>(a: &[T], b: &[T]) -> Vec<T> {
    let mut res = Vec::with_capacity(a.len() + b.len());
    res.extend_from_slice(a);
    res.extend_from_slice(b);
    res
}
