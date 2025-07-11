fn main() {
    fn boom(a: i32, b: i32) -> bool {
        println!("boom");
        return true;
    }
    println!("{}", false && boom(1, 2));
    println!("{}", true || boom(1, 2));
}
