fn append<T: Clone>(mut v: Vec<T>, item: T) -> Vec<T> {
    v.push(item);
    v
}

fn main() {
    let mut a = vec![1, 2];
    println!("{:?}", append(a, 3));
}
