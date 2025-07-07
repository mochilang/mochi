fn main() {
    let data = vec![1, 2];
    let flag = data.iter().any(|&x| x == 1);
    println!("{}", flag);
}
