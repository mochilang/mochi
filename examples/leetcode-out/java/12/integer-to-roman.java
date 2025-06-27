public class Main {
  static String intToRoman(int num) {
    Object[] values = new int[] {1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1};
    Object[] symbols =
        new String[] {"M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"};
    String result = "";
    int i = 0;
    while ((num > 0)) {
      while ((num >= values[i])) {
        result = (result + symbols[i]);
        num = (num - values[i]);
      }
      i = (i + 1);
    }
    return result;
  }

  static void test_example_1() {
    expect((intToRoman(3) == "III"));
  }

  static void test_example_2() {
    expect((intToRoman(58) == "LVIII"));
  }

  static void test_example_3() {
    expect((intToRoman(1994) == "MCMXCIV"));
  }

  static void test_small_numbers() {
    expect((intToRoman(4) == "IV"));
    expect((intToRoman(9) == "IX"));
  }

  public static void main(String[] args) {
    test_example_1();
    test_example_2();
    test_example_3();
    test_small_numbers();
  }

  static void expect(boolean cond) {
    if (!cond) throw new RuntimeException("expect failed");
  }
}
