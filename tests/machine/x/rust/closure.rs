fn main() {
    fn makeAdder(n: i32) -> Box<dyn Fn(i32) -> i32> {
        return Box::new(move |x: i32| x + n);
    }
    let add10 = makeAdder(10);
    println!("{}", add10(7));
}
