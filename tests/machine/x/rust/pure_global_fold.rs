fn main() {
    let k = 2;
    let inc = move |x: i32| -> i32 {
        return x + k;
    };
    println!("{:?}", inc(3));
}
