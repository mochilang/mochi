fn main() {
    let a = vec![1, 2, 3];
    println!("{:?}", &a[1..3]);
    println!("{:?}", &a[0..2]);
    let s = "hello";
    println!("{}", &s[1..4]);
}
