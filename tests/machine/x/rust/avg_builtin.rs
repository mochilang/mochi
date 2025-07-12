fn avg<T>(v: &[T]) -> f64 where T: Into<f64> + Copy {
    let sum: f64 = v.iter().map(|&x| x.into()).sum();
    sum / v.len() as f64
}

fn main() {
    println!("{}", avg(&vec![1, 2, 3]));
}
