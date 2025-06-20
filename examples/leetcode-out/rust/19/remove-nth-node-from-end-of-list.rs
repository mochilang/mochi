fn removeNthFromEnd(nums: Vec<i32>, n: i32) -> Vec<i32> {
    let mut idx = nums.len() as i32 - n;
    let mut result = vec![];
    let mut i = 0;
    while i < nums.len() as i32 {
        if i != idx {
            result = _concat(&result, &vec![nums[i as usize]]);
        }
        i = i + 1;
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
