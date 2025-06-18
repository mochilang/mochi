fn reverse(x: i32) -> i32 {
    let mut sign = 1;
    let mut n = x;
    if n < 0 {
        sign = -1;
        n = -n;
    }
    let mut rev = 0;
    while n != 0 {
        let mut digit = n % 10;
        rev = rev * 10 + digit;
        n = n / 10;
    }
    rev = rev * sign;
    if rev < (-2147483647 - 1) || rev > 2147483647 {
        return 0;
    }
    return rev;
}

fn main() {
}

