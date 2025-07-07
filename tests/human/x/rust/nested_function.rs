fn outer(x: i32) -> i32 {
    fn inner(x: i32, y: i32) -> i32 { x + y }
    inner(x, 5)
}

fn main() {
    println!("{}", outer(3));
}
