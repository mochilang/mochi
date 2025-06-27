fn mergeKLists(lists: Vec<Vec<i64>>) -> Vec<i64> {
    let mut k = lists.len() as i64;
    let mut indices: Vec<i64> = vec![];
    let mut i = 0;
    while i < k {
        indices = {
            let a = &indices;
            let b = &vec![0];
            let mut res = Vec::with_capacity(a.len() + b.len());
            res.extend_from_slice(a);
            res.extend_from_slice(b);
            res
        };
        i = i + 1;
    }
    let mut result: Vec<i64> = vec![];
    while true {
        let mut best = 0;
        let mut bestList = -1;
        let mut found = false;
        let mut j = 0;
        while j < k {
            let mut idx = indices[j as usize];
            if idx < lists[j as usize].len() as i64 {
                let mut val = lists[j as usize][idx as usize];
                if !found || val < best {
                    best = val;
                    bestList = j;
                    found = true;
                }
            }
            j = j + 1;
        }
        if !found {
            break;
        }
        result = {
            let a = &result;
            let b = &vec![best];
            let mut res = Vec::with_capacity(a.len() + b.len());
            res.extend_from_slice(a);
            res.extend_from_slice(b);
            res
        };
        indices[bestList as usize] = indices[bestList as usize] + 1;
    }
    return result;
}

fn test_example_1() {
    expect(
        mergeKLists(vec![vec![1, 4, 5], vec![1, 3, 4], vec![2, 6]]) == vec![1, 1, 2, 3, 4, 4, 5, 6],
    );
}

fn test_example_2() {
    expect(mergeKLists(vec![]) == vec![]);
}

fn test_example_3() {
    expect(mergeKLists(vec![vec![]]) == vec![]);
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
