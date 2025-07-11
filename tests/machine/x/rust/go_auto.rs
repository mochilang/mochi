fn main() {
    mod testpkg {
        pub fn Add(a: i32, b: i32) -> i32 { a + b }
        pub const Pi: f64 = 3.14;
        pub const Answer: i32 = 42;
    }
    println!("{:?}", testpkg::Add(2, 3));
    println!("{:?}", testpkg::Pi);
    println!("{:?}", testpkg::Answer);
}
