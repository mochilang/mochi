fn addTwoNumbers(l1: Vec<i32>, l2: Vec<i32>) -> Vec<i32> {
    let mut i = 0;
    let mut j = 0;
    let mut carry = 0;
    let mut result: Vec<i32> = vec![];
    while i < l1.len() as i32 || j < l2.len() as i32 || carry > 0 {
        let mut x = 0;
        if i < l1.len() as i32 {
            x = l1[(i) as usize];
            i = i + 1;
        }
        let mut y = 0;
        if j < l2.len() as i32 {
            y = l2[(j) as usize];
            j = j + 1;
        }
        let mut sum = x + y + carry;
        let mut digit = sum % 10;
        carry = sum / 10;
        result = _concat(&result, &vec![digit]);
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
