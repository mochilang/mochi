fn sum(v: &[i32]) -> i32 {
    v.iter().sum()
}

fn main() {
    println!("{:?}", sum(&vec![1, 2, 3]));
}
