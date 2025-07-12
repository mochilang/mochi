fn main() {
    println!("{:?}", vec![1, 2, 3][1..3].to_vec());
    println!("{:?}", vec![1, 2, 3][0..2].to_vec());
    println!("{}", &"hello"[1..4]);
}
