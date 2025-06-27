fn reverse(x: i64) -> i64 {
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

fn test_example_1() {
    expect(reverse(123) == 321);
}

fn test_example_2() {
    expect(reverse(-123) == (-321));
}

fn test_example_3() {
    expect(reverse(120) == 21);
}

fn test_overflow() {
    expect(reverse(1534236469) == 0);
}

fn main() {
    test_example_1();
    test_example_2();
    test_example_3();
    test_overflow();
}

fn expect(cond: bool) {
    if !cond {
        panic!("expect failed");
    }
}
