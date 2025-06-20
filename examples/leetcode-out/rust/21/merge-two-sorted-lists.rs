fn mergeTwoLists(l1: Vec<i32>, l2: Vec<i32>) -> Vec<i32> {
    let mut i = 0;
    let mut j = 0;
    let mut result = vec![];
    while i < l1.len() as i32 && j < l2.len() as i32 {
        if l1[i as usize] <= l2[j as usize] {
            result = _concat(&result, &vec![l1[i as usize]]);
            i = i + 1;
        } else {
            result = _concat(&result, &vec![l2[j as usize]]);
            j = j + 1;
        }
    }
    while i < l1.len() as i32 {
        result = _concat(&result, &vec![l1[i as usize]]);
        i = i + 1;
    }
    while j < l2.len() as i32 {
        result = _concat(&result, &vec![l2[j as usize]]);
        j = j + 1;
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
