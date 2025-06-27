fn findMedianSortedArrays(nums1: Vec<i64>, nums2: Vec<i64>) -> f64 {
    let mut merged: Vec<i64> = vec![];
    let mut i = 0;
    let mut j = 0;
    while i < nums1.len() as i64 || j < nums2.len() as i64 {
        if j >= nums2.len() as i64 {
            merged = {
                let a = &merged;
                let b = &vec![nums1[i as usize]];
                let mut res = Vec::with_capacity(a.len() + b.len());
                res.extend_from_slice(a);
                res.extend_from_slice(b);
                res
            };
            i = i + 1;
        } else if i >= nums1.len() as i64 {
            merged = {
                let a = &merged;
                let b = &vec![nums2[j as usize]];
                let mut res = Vec::with_capacity(a.len() + b.len());
                res.extend_from_slice(a);
                res.extend_from_slice(b);
                res
            };
            j = j + 1;
        } else if nums1[i as usize] <= nums2[j as usize] {
            merged = {
                let a = &merged;
                let b = &vec![nums1[i as usize]];
                let mut res = Vec::with_capacity(a.len() + b.len());
                res.extend_from_slice(a);
                res.extend_from_slice(b);
                res
            };
            i = i + 1;
        } else {
            merged = {
                let a = &merged;
                let b = &vec![nums2[j as usize]];
                let mut res = Vec::with_capacity(a.len() + b.len());
                res.extend_from_slice(a);
                res.extend_from_slice(b);
                res
            };
            j = j + 1;
        }
    }
    let mut total = merged.len() as i64;
    if total % 2 == 1 {
        return (merged[(total / 2) as usize] as f64);
    }
    let mut mid1 = merged[(total / 2 - 1) as usize];
    let mut mid2 = merged[(total / 2) as usize];
    return ((mid1 + mid2) as f64) / 2.0;
}

fn test_example_1() {
    expect(findMedianSortedArrays(vec![1, 3], vec![2]) == 2.0);
}

fn test_example_2() {
    expect(findMedianSortedArrays(vec![1, 2], vec![3, 4]) == 2.5);
}

fn test_empty_first() {
    expect(findMedianSortedArrays((vec![] as Vec<i64>), vec![1]) == 1.0);
}

fn test_empty_second() {
    expect(findMedianSortedArrays(vec![2], (vec![] as Vec<i64>)) == 2.0);
}

fn main() {
    test_example_1();
    test_example_2();
    test_empty_first();
    test_empty_second();
}

fn expect(cond: bool) {
    if !cond {
        panic!("expect failed");
    }
}
