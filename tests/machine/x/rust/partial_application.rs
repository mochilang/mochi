fn main() {
    fn add(a: i32, b: i32) -> i32 {
        return a + b;
    }
    let add5 = |tmp1| add(5, tmp1);
    println!("{:?}", add5(3));
}
