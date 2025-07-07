fn sum_rec(n: i32, acc: i32) -> i32 {
    if n == 0 {
        acc
    } else {
        sum_rec(n - 1, acc + n)
    }
}

fn main() {
    println!("{}", sum_rec(10, 0));
}
