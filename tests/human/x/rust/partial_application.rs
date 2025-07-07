fn add(a: i32, b: i32) -> i32 { a + b }

fn main() {
    let add5 = |b| add(5, b);
    println!("{}", add5(3));
}
