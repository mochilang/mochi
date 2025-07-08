fn main() {
    let square = Box::new(move |x: i32| x * x);
    println!("{:?}", square(6));
}
