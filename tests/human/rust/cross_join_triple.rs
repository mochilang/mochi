fn main() {
    let nums = vec![1, 2];
    let letters = vec!["A", "B"];
    let bools = vec![true, false];

    println!("--- Cross Join of three lists ---");
    for n in &nums {
        for l in &letters {
            for b in &bools {
                println!("{} {} {}", n, l, b);
            }
        }
    }
}
