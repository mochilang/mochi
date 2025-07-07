fn boom(_a: i32, _b: i32) -> bool {
    println!("boom");
    true
}
fn main() {
    println!("{}", false && boom(1, 2));
    println!("{}", true || boom(1, 2));
}
