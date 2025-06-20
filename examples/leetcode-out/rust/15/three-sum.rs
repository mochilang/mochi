fn threeSum(nums: Vec<i32>) -> Vec<Vec<i32>> {
    let mut sorted = {
    let mut _res = Vec::new();
    for x in nums {
        _res.push(x);
    }
    _res
};
    let mut n = sorted.len() as i32;
    let mut res: Vec<Vec<i32>> = vec![];
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
                res = _concat(&res, &vec![vec![sorted[i as usize], sorted[left as usize], sorted[right as usize]]]);
                left = left + 1;
                while left < right && sorted[left as usize] == sorted[(left - 1) as usize] {
                    left = left + 1;
                }
                right = right - 1;
                while left < right && sorted[right as usize] == sorted[(right + 1) as usize] {
                    right = right - 1;
                }
            } else 
            if sum < 0 {
                left = left + 1;
            } else {
                right = right - 1;
            }
        }
        i = i + 1;
    }
    return res;
}

fn main() {
}

fn _concat<T: Clone>(a: &[T], b: &[T]) -> Vec<T> {
    let mut res = Vec::with_capacity(a.len() + b.len());
    res.extend_from_slice(a);
    res.extend_from_slice(b);
    res
}
