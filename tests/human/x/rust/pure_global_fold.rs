static K: i32 = 2;
fn inc(x: i32) -> i32 { x + K }
fn main() {
    println!("{}", inc(3));
}
