public class Main {
  static int lengthOfLongestSubstring(String s) {
    int n = s.length();
    int start = 0;
    int best = 0;
    int i = 0;
    while ((i < n)) {
      int j = start;
      while ((j < i)) {
        if ((_indexString(s, j) == _indexString(s, i))) {
          start = (j + 1);
          break;
        }
        j = (j + 1);
      }
      int length = ((i - start) + 1);
      if ((length > best)) {
        best = length;
      }
      i = (i + 1);
    }
    return best;
  }

  static void test_example_1() {
    expect((lengthOfLongestSubstring("abcabcbb") == 3));
  }

  static void test_example_2() {
    expect((lengthOfLongestSubstring("bbbbb") == 1));
  }

  static void test_example_3() {
    expect((lengthOfLongestSubstring("pwwkew") == 3));
  }

  static void test_empty_string() {
    expect((lengthOfLongestSubstring("") == 0));
  }

  public static void main(String[] args) {
    test_example_1();
    test_example_2();
    test_example_3();
    test_empty_string();
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
