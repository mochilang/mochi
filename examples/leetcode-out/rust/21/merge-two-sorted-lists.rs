fn mergeTwoLists(l1: Vec<i64>, l2: Vec<i64>) -> Vec<i64> {
    let mut i = 0;
    let mut j = 0;
    let mut result = vec![];
    while i < l1.len() as i64 && j < l2.len() as i64 {
        if l1[i as usize] <= l2[j as usize] {
            result = {
                let a = &result;
                let b = &vec![l1[i as usize]];
                let mut res = Vec::with_capacity(a.len() + b.len());
                res.extend_from_slice(a);
                res.extend_from_slice(b);
                res
            };
            i = i + 1;
        } else {
            result = {
                let a = &result;
                let b = &vec![l2[j as usize]];
                let mut res = Vec::with_capacity(a.len() + b.len());
                res.extend_from_slice(a);
                res.extend_from_slice(b);
                res
            };
            j = j + 1;
        }
    }
    while i < l1.len() as i64 {
        result = {
            let a = &result;
            let b = &vec![l1[i as usize]];
            let mut res = Vec::with_capacity(a.len() + b.len());
            res.extend_from_slice(a);
            res.extend_from_slice(b);
            res
        };
        i = i + 1;
    }
    while j < l2.len() as i64 {
        result = {
            let a = &result;
            let b = &vec![l2[j as usize]];
            let mut res = Vec::with_capacity(a.len() + b.len());
            res.extend_from_slice(a);
            res.extend_from_slice(b);
            res
        };
        j = j + 1;
    }
    return result;
}

fn test_example_1() {
    expect(mergeTwoLists(vec![1, 2, 4], vec![1, 3, 4]) == vec![1, 1, 2, 3, 4, 4]);
}

fn test_example_2() {
    expect(mergeTwoLists(vec![], vec![]) == vec![]);
}

fn test_example_3() {
    expect(mergeTwoLists(vec![], vec![0]) == vec![0]);
}

fn test_different_lengths() {
    expect(mergeTwoLists(vec![1, 5, 7], vec![2, 3, 4, 6, 8]) == vec![1, 2, 3, 4, 5, 6, 7, 8]);
}

fn test_one_list_empty() {
    expect(mergeTwoLists(vec![1, 2, 3], vec![]) == vec![1, 2, 3]);
}

fn main() {
    test_example_1();
    test_example_2();
    test_example_3();
    test_different_lengths();
    test_one_list_empty();
}

fn expect(cond: bool) {
    if !cond {
        panic!("expect failed");
    }
}
