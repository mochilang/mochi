fn main() {
    fn outer(x: i32) -> i32 {
        let inner = move |y: i32| -> i32 {
            return x + y;
        };
        return inner(5);
    }
    println!("{:?}", outer(3));
}
