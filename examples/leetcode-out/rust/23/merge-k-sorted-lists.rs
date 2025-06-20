fn mergeKLists(lists: Vec<Vec<i32>>) -> Vec<i32> {
    let mut k = lists.len() as i32;
    let mut indices: Vec<i32> = vec![];
    let mut i = 0;
    while i < k {
        indices = _concat(&indices, &vec![0]);
        i = i + 1;
    }
    let mut result: Vec<i32> = vec![];
    while true {
        let mut best = 0;
        let mut bestList = -1;
        let mut found = false;
        let mut j = 0;
        while j < k {
            let mut idx = indices[j as usize];
            if idx < lists[j as usize].len() as i32 {
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
        result = _concat(&result, &vec![best]);
        indices[bestList as usize] = indices[bestList as usize] + 1;
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
