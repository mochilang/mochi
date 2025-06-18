fn findMedianSortedArrays(nums1: Vec<i32>, nums2: Vec<i32>) -> f64 {
    let mut merged = vec![];
    let mut i = 0;
    let mut j = 0;
    while i < nums1.len() as i32 || j < nums2.len() as i32 {
        if j >= nums2.len() as i32 {
            merged = _concat(&merged, &vec![nums1[(i) as usize]]);
            i = i + 1;
        } else 
        if i >= nums1.len() as i32 {
            merged = _concat(&merged, &vec![nums2[(j) as usize]]);
            j = j + 1;
        } else 
        if nums1[(i) as usize] <= nums2[(j) as usize] {
            merged = _concat(&merged, &vec![nums1[(i) as usize]]);
            i = i + 1;
        } else {
            merged = _concat(&merged, &vec![nums2[(j) as usize]]);
            j = j + 1;
        }
    }
    let mut total = merged.len() as i32;
    if total % 2 == 1 {
        return (merged[(total / 2) as usize] as f64);
    }
    let mut mid1 = merged[(total / 2 - 1) as usize];
    let mut mid2 = merged[(total / 2) as usize];
    return ((mid1 + mid2) as f64) / 2.0;
}

fn main() {
}

fn _concat<T: Clone>(a: &[T], b: &[T]) -> Vec<T> {
    let mut res = Vec::with_capacity(a.len() + b.len());
    res.extend_from_slice(a);
    res.extend_from_slice(b);
    res
}
