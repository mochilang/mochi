fn swapPairs(nums: Vec<i32>) -> Vec<i32> {
    let mut i = 0;
    let mut result = vec![];
    while i < nums.len() as i32 {
        if i + 1 < nums.len() as i32 {
            result = _concat(&result, &vec![nums[(i + 1) as usize], nums[i as usize]]);
        } else {
            result = _concat(&result, &vec![nums[i as usize]]);
        }
        i = i + 2;
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
