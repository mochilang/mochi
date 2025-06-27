public class Main {
  static String longestCommonPrefix(Object[] strs) {
    if ((strs.length == 0)) {
      return "";
    }
    String prefix = strs[0];
    for (int i = 1; i < strs.length; i++) {
      int j = 0;
      String current = strs[i];
      while (((j < prefix.length()) && (j < current.length()))) {
        if ((_indexString(prefix, j) != _indexString(current, j))) {
          break;
        }
        j = (j + 1);
      }
      prefix = _sliceString(prefix, 0, j);
      if ((prefix == "")) {
        break;
      }
    }
    return prefix;
  }

  static void test_example_1() {
    expect((longestCommonPrefix(new String[] {"flower", "flow", "flight"}) == "fl"));
  }

  static void test_example_2() {
    expect((longestCommonPrefix(new String[] {"dog", "racecar", "car"}) == ""));
  }

  static void test_single_string() {
    expect((longestCommonPrefix(new String[] {"single"}) == "single"));
  }

  static void test_no_common_prefix() {
    expect((longestCommonPrefix(new String[] {"a", "b", "c"}) == ""));
  }

  public static void main(String[] args) {
    test_example_1();
    test_example_2();
    test_single_string();
    test_no_common_prefix();
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
