fn main() {
    let xs = vec![1, 2, 3];
    println!("{:?}", xs.contains(&2));
    println!("{:?}", !(xs.contains(&5)));
}
