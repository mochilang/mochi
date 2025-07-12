fn min<T: PartialOrd + Copy>(v: &[T]) -> T {
    *v.iter().min_by(|a,b| a.partial_cmp(b).unwrap()).unwrap()
}

fn max<T: PartialOrd + Copy>(v: &[T]) -> T {
    *v.iter().max_by(|a,b| a.partial_cmp(b).unwrap()).unwrap()
}

fn main() {
    let nums = vec![3, 1, 4];
    println!("{}", min(&nums));
    println!("{}", max(&nums));
}
