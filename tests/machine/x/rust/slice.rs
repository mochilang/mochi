fn main() {
    println!("{:?}", vec![1, 2, 3][1 as usize..3 as usize].to_vec());
    println!("{:?}", vec![1, 2, 3][0 as usize..2 as usize].to_vec());
    println!("{}", &"hello"[1 as usize..4 as usize]);
}
