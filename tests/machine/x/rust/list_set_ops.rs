fn _union<T: Eq + std::hash::Hash + Clone>(a: Vec<T>, b: Vec<T>) -> Vec<T> {
    use std::collections::HashSet;
    let mut set: HashSet<T> = a.into_iter().collect();
    set.extend(b.into_iter());
    set.into_iter().collect()
}

fn _union_all<T: Clone>(mut a: Vec<T>, b: Vec<T>) -> Vec<T> {
    a.extend(b);
    a
}

fn _except<T: Eq + std::hash::Hash + Clone>(a: Vec<T>, b: Vec<T>) -> Vec<T> {
    use std::collections::HashSet;
    let set: HashSet<T> = b.into_iter().collect();
    a.into_iter().filter(|x| !set.contains(x)).collect()
}

fn _intersect<T: Eq + std::hash::Hash + Clone>(a: Vec<T>, b: Vec<T>) -> Vec<T> {
    use std::collections::HashSet;
    let set: HashSet<T> = b.into_iter().collect();
    a.into_iter().filter(|x| set.contains(x)).collect()
}

fn main() {
    println!("{:?}", _union(vec![1, 2], vec![2, 3]));
    println!("{:?}", _except(vec![1, 2, 3], vec![2]));
    println!("{:?}", _intersect(vec![1, 2, 3], vec![2, 4]));
    println!("{:?}", _union_all(vec![1, 2], vec![2, 3]).len());
}
