fn main() {
    fn outer(x: i32) -> i32 {
        fn inner(y: i32) -> i32 {
            return x + y;
        }
        return inner(5);
    }
    println!("{:?}", outer(3));
}
