public class Main {
  static int romanToInt(String s) {
    java.util.Map<String, Integer> values =
        new java.util.HashMap<>(
            java.util.Map.of("I", 1, "V", 5, "X", 10, "L", 50, "C", 100, "D", 500, "M", 1000));
    int total = 0;
    int i = 0;
    int n = s.length();
    while ((i < n)) {
      int curr = values.get(_indexString(s, i));
      if (((i + 1) < n)) {
        int next = values.get(_indexString(s, (i + 1)));
        if ((curr < next)) {
          total = ((total + next) - curr);
          i = (i + 2);
          continue;
        }
      }
      total = (total + curr);
      i = (i + 1);
    }
    return total;
  }

  static void test_example_1() {
    expect((romanToInt("III") == 3));
  }

  static void test_example_2() {
    expect((romanToInt("LVIII") == 58));
  }

  static void test_example_3() {
    expect((romanToInt("MCMXCIV") == 1994));
  }

  static void test_subtractive() {
    expect((romanToInt("IV") == 4));
    expect((romanToInt("IX") == 9));
  }

  static void test_tens() {
    expect((romanToInt("XL") == 40));
    expect((romanToInt("XC") == 90));
  }

  public static void main(String[] args) {
    test_example_1();
    test_example_2();
    test_example_3();
    test_subtractive();
    test_tens();
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
