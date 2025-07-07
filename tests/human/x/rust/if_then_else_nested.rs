fn main() {
    let x = 8;
    let msg = if x > 10 {
        "big"
    } else if x > 5 {
        "medium"
    } else {
        "small"
    };
    println!("{}", msg);
}
