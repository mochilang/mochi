fn main() {
    let nums = vec![1, 2, 3];
    let letters = vec!["A", "B"];

    println!("--- Even pairs ---");
    for n in &nums {
        if n % 2 == 0 {
            for l in &letters {
                println!("{} {}", n, l);
            }
        }
    }
}
