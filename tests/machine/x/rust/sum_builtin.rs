fn sum<T>(v: &[T]) -> T where T: std::iter::Sum<T> + Copy {
    v.iter().copied().sum()
}

fn main() {
    println!("{}", sum(&vec![1, 2, 3]));
}
