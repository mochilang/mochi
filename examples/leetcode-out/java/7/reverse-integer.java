public class Main {
  static int reverse(int x) {
    int sign = 1;
    int n = x;
    if ((n < 0)) {
      sign = (-1);
      n = (-n);
    }
    int rev = 0;
    while ((n != 0)) {
      int digit = (n % 10);
      rev = ((rev * 10) + digit);
      n = (n / 10);
    }
    rev = (rev * sign);
    if (((rev < ((-2147483647) - 1)) || (rev > 2147483647))) {
      return 0;
    }
    return rev;
  }

  static void test_example_1() {
    expect((reverse(123) == 321));
  }

  static void test_example_2() {
    expect((reverse((-123)) == (-321)));
  }

  static void test_example_3() {
    expect((reverse(120) == 21));
  }

  static void test_overflow() {
    expect((reverse(1534236469) == 0));
  }

  public static void main(String[] args) {
    test_example_1();
    test_example_2();
    test_example_3();
    test_overflow();
  }

  static void expect(boolean cond) {
    if (!cond) throw new RuntimeException("expect failed");
  }
}
