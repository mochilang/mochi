public class Main {
  static int strStr(String haystack, String needle) {
    int n = haystack.length();
    int m = needle.length();
    if ((m == 0)) {
      return 0;
    }
    if ((m > n)) {
      return (-1);
    }
    for (int i = 0; i < ((n - m) + 1); i++) {
      int j = 0;
      while ((j < m)) {
        if ((_indexString(haystack, (i + j)) != _indexString(needle, j))) {
          break;
        }
        j = (j + 1);
      }
      if ((j == m)) {
        return i;
      }
    }
    return (-1);
  }

  static void test_example_1() {
    expect((strStr("sadbutsad", "sad") == 0));
  }

  static void test_example_2() {
    expect((strStr("leetcode", "leeto") == (-1)));
  }

  static void test_empty_needle() {
    expect((strStr("abc", "") == 0));
  }

  static void test_needle_at_end() {
    expect((strStr("hello", "lo") == 3));
  }

  public static void main(String[] args) {
    test_example_1();
    test_example_2();
    test_empty_needle();
    test_needle_at_end();
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
