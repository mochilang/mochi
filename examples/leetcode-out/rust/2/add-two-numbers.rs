fn addTwoNumbers(l1: Vec<i64>, l2: Vec<i64>) -> Vec<i64> {
    let mut i = 0;
    let mut j = 0;
    let mut carry = 0;
    let mut result: Vec<i64> = vec![];
    while i < l1.len() as i64 || j < l2.len() as i64 || carry > 0 {
        let mut x = 0;
        if i < l1.len() as i64 {
            x = l1[i as usize];
            i = i + 1;
        }
        let mut y = 0;
        if j < l2.len() as i64 {
            y = l2[j as usize];
            j = j + 1;
        }
        let mut sum = x + y + carry;
        let mut digit = sum % 10;
        carry = sum / 10;
        result = {
            let a = &result;
            let b = &vec![digit];
            let mut res = Vec::with_capacity(a.len() + b.len());
            res.extend_from_slice(a);
            res.extend_from_slice(b);
            res
        };
    }
    return result;
}

fn test_example_1() {
    expect(addTwoNumbers(vec![2, 4, 3], vec![5, 6, 4]) == vec![7, 0, 8]);
}

fn test_example_2() {
    expect(addTwoNumbers(vec![0], vec![0]) == vec![0]);
}

fn test_example_3() {
    expect(
        addTwoNumbers(vec![9, 9, 9, 9, 9, 9, 9], vec![9, 9, 9, 9]) == vec![8, 9, 9, 9, 0, 0, 0, 1],
    );
}

fn main() {
    test_example_1();
    test_example_2();
    test_example_3();
}

fn expect(cond: bool) {
    if !cond {
        panic!("expect failed");
    }
}
