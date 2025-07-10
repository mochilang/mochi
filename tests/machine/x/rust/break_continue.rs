fn main() {
    let numbers = vec![1, 2, 3, 4, 5, 6, 7, 8, 9];
    for n in numbers {
        if n % 2 == 0 {
            continue;
        }
        if n > 7 {
            break;
        }
        println!("{} {:?}", "odd number:", n);
    }
}
