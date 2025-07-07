fn main() {
    println!("{:?}", vec![1, 2] union vec![2, 3]);
    println!("{:?}", vec![1, 2, 3] except vec![2]);
    println!("{:?}", vec![1, 2, 3] intersect vec![2, 4]);
    println!("{:?}", len(vec![1, 2] union vec![2, 3]));
}
