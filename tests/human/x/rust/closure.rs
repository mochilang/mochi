fn make_adder(n: i32) -> impl Fn(i32) -> i32 {
    move |x| x + n
}

fn main() {
    let add10 = make_adder(10);
    println!("{}", add10(7));
}
