fn boom() -> bool {
    println!("boom");
    true
}

fn main() {
    println!("{}", (1 < 2) && (2 < 3) && (3 < 4));
    println!("{}", (1 < 2) && (2 > 3) && boom());
    println!("{}", (1 < 2) && (2 < 3) && (3 > 4) && boom());
}
