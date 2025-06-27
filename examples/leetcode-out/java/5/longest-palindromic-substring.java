public class Main {
  static int expand(String s, int left, int right) {
    int l = left;
    int r = right;
    int n = s.length();
    while (((l >= 0) && (r < n))) {
      if ((_indexString(s, l) != _indexString(s, r))) {
        break;
      }
      l = (l - 1);
      r = (r + 1);
    }
    return ((r - l) - 1);
  }

  static String longestPalindrome(String s) {
    if ((s.length() <= 1)) {
      return s;
    }
    int start = 0;
    int end = 0;
    int n = s.length();
    for (int i = 0; i < n; i++) {
      int len1 = expand(s, i, i);
      int len2 = expand(s, i, (i + 1));
      int l = len1;
      if ((len2 > len1)) {
        l = len2;
      }
      if ((l > (end - start))) {
        start = (i - ((l - 1) / 2));
        end = (i + (l / 2));
      }
    }
    return _sliceString(s, start, (end + 1));
  }

  static void test_example_1() {
    String ans = longestPalindrome("babad");
    expect(((ans == "bab") || (ans == "aba")));
  }

  static void test_example_2() {
    expect((longestPalindrome("cbbd") == "bb"));
  }

  static void test_single_char() {
    expect((longestPalindrome("a") == "a"));
  }

  static void test_two_chars() {
    String ans = longestPalindrome("ac");
    expect(((ans == "a") || (ans == "c")));
  }

  public static void main(String[] args) {
    test_example_1();
    test_example_2();
    test_single_char();
    test_two_chars();
  }

  static String _indexString(String s, int i) {
    char[] runes = s.toCharArray();
    if (i < 0) i += runes.length;
    if (i < 0 || i >= runes.length) throw new RuntimeException("index out of range");
    return String.valueOf(runes[i]);
  }

  static String _sliceString(String s, int i, int j) {
    int start = i;
    int end = j;
    int n = s.length();
    if (start < 0) start += n;
    if (end < 0) end += n;
    if (start < 0) start = 0;
    if (end > n) end = n;
    if (end < start) end = start;
    return s.substring(start, end);
  }

  static void expect(boolean cond) {
    if (!cond) throw new RuntimeException("expect failed");
  }
}
