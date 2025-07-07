fn main() {
    let mut a = vec![1, 2];
    a.push(3);
    for (i, val) in a.iter().enumerate() {
        if i > 0 {
            print!(" ");
        }
        print!("{}", val);
    }
    println!();
}
