fn reverseKGroup(nums: Vec<i32>, k: i32) -> Vec<i32> {
    let mut n = nums.len() as i32;
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
                result = _concat(&result, &vec![nums[j as usize]]);
                j = j - 1;
            }
        } else {
            let mut j = i;
            while j < n {
                result = _concat(&result, &vec![nums[j as usize]]);
                j = j + 1;
            }
        }
        i = i + k;
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
