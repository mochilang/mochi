fn avg(v: &[i32]) -> f64 {
    let sum: i32 = v.iter().sum();
    sum as f64 / v.len() as f64
}

fn main() {
    println!("{}", avg(&vec![1, 2, 3]));
}
