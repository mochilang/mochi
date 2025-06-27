public class Main {
  static int divide(int dividend, int divisor) {
    if (((dividend == ((-2147483647) - 1)) && (divisor == (-1)))) {
      return 2147483647;
    }
    boolean negative = false;
    if ((dividend < 0)) {
      negative = (!negative);
      dividend = (-dividend);
    }
    if ((divisor < 0)) {
      negative = (!negative);
      divisor = (-divisor);
    }
    int quotient = 0;
    while ((dividend >= divisor)) {
      int temp = divisor;
      int multiple = 1;
      while ((dividend >= (temp + temp))) {
        temp = (temp + temp);
        multiple = (multiple + multiple);
      }
      dividend = (dividend - temp);
      quotient = (quotient + multiple);
    }
    if (negative) {
      quotient = (-quotient);
    }
    if ((quotient > 2147483647)) {
      return 2147483647;
    }
    if ((quotient < ((-2147483647) - 1))) {
      return (-2147483648);
    }
    return quotient;
  }

  static void test_example_1() {
    expect((divide(10, 3) == 3));
  }

  static void test_example_2() {
    expect((divide(7, (-3)) == (-2)));
  }

  static void test_overflow() {
    expect((divide((-2147483648), (-1)) == 2147483647));
  }

  static void test_divide_by_1() {
    expect((divide(12345, 1) == 12345));
  }

  static void test_negative_result() {
    expect((divide((-15), 2) == (-7)));
  }

  public static void main(String[] args) {
    test_example_1();
    test_example_2();
    test_overflow();
    test_divide_by_1();
    test_negative_result();
  }

  static void expect(boolean cond) {
    if (!cond) throw new RuntimeException("expect failed");
  }
}
