fn min(v: &[i32]) -> i32 {
    *v.iter().min().unwrap()
}

fn max(v: &[i32]) -> i32 {
    *v.iter().max().unwrap()
}

fn main() {
    let nums = vec![3, 1, 4];
    println!("{}", min(&nums));
    println!("{}", max(&nums));
}
