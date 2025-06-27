public class Main {
  static boolean isPalindrome(int x) {
    if ((x < 0)) {
      return false;
    }
    String s = String.valueOf(x);
    int n = s.length();
    for (int i = 0; i < (n / 2); i++) {
      if ((_indexString(s, i) != _indexString(s, ((n - 1) - i)))) {
        return false;
      }
    }
    return true;
  }

  static void test_example_1() {
    expect((isPalindrome(121) == true));
  }

  static void test_example_2() {
    expect((isPalindrome((-121)) == false));
  }

  static void test_example_3() {
    expect((isPalindrome(10) == false));
  }

  static void test_zero() {
    expect((isPalindrome(0) == true));
  }

  public static void main(String[] args) {
    test_example_1();
    test_example_2();
    test_example_3();
    test_zero();
  }

  static String _indexString(String s, int i) {
    char[] runes = s.toCharArray();
    if (i < 0) i += runes.length;
    if (i < 0 || i >= runes.length) throw new RuntimeException("index out of range");
    return String.valueOf(runes[i]);
  }

  static void expect(boolean cond) {
    if (!cond) throw new RuntimeException("expect failed");
  }
}
