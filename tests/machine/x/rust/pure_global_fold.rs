fn main() {
    let mut k = 2;
    fn inc(x: i32) -> i32 {
        return x + k;
    }
    println!("{:?}", inc(3));
}
