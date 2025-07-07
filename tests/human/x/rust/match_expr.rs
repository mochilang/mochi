fn main() {
    let x = 2;
    let label = match x {
        1 => "one",
        2 => "two",
        3 => "three",
        _ => "unknown",
    };
    println!("{}", label);
}
