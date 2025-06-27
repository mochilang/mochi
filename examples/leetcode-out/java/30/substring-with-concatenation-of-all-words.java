public class Main {
  static Object[] findSubstring(String s, Object[] words) {
    if ((words.length == 0)) {
      return new int[] {};
    }
    int wordLen = words[0].length;
    int wordCount = words.length;
    int totalLen = (wordLen * wordCount);
    if ((s.length() < totalLen)) {
      return new int[] {};
    }
    java.util.Map<String, Integer> freq = new java.util.HashMap<>();
    for (var w : words) {
      if (_in(w, freq)) {
        freq.put(w, (freq.get(w) + 1));
      } else {
        freq.put(w, 1);
      }
    }
    Object[] result = new int[] {};
    for (int offset = 0; offset < wordLen; offset++) {
      int left = offset;
      int count = 0;
      java.util.Map<String, Integer> seen = new java.util.HashMap<>();
      int j = offset;
      while (((j + wordLen) <= s.length())) {
        String word = _sliceString(s, j, (j + wordLen));
        j = (j + wordLen);
        if (_in(word, freq)) {
          if (_in(word, seen)) {
            seen.put(word, (seen.get(word) + 1));
          } else {
            seen.put(word, 1);
          }
          count = (count + 1);
          while ((seen.get(word) > freq.get(word))) {
            String lw = _sliceString(s, left, (left + wordLen));
            seen.put(lw, (seen.get(lw) - 1));
            left = (left + wordLen);
            count = (count - 1);
          }
          if ((count == wordCount)) {
            result = _concat(result, new int[] {left});
            String lw = _sliceString(s, left, (left + wordLen));
            seen.put(lw, (seen.get(lw) - 1));
            left = (left + wordLen);
            count = (count - 1);
          }
        } else {
          seen = new java.util.HashMap<>();
          count = 0;
          left = j;
        }
      }
    }
    return result;
  }

  static void test_example_1() {
    expect((findSubstring("barfoothefoobarman", new String[] {"foo", "bar"}) == new int[] {0, 9}));
  }

  static void test_example_2() {
    expect(
        (findSubstring("wordgoodgoodgoodbestword", new String[] {"word", "good", "best", "word"})
            == new Object[] {}));
  }

  static void test_example_3() {
    expect(
        (findSubstring("barfoofoobarthefoobarman", new String[] {"bar", "foo", "the"})
            == new int[] {6, 9, 12}));
  }

  public static void main(String[] args) {
    test_example_1();
    test_example_2();
    test_example_3();
  }

  static int[] _concat(int[] a, int[] b) {
    int[] res = new int[a.length + b.length];
    System.arraycopy(a, 0, res, 0, a.length);
    System.arraycopy(b, 0, res, a.length, b.length);
    return res;
  }

  static double[] _concat(double[] a, double[] b) {
    double[] res = new double[a.length + b.length];
    System.arraycopy(a, 0, res, 0, a.length);
    System.arraycopy(b, 0, res, a.length, b.length);
    return res;
  }

  static boolean[] _concat(boolean[] a, boolean[] b) {
    boolean[] res = new boolean[a.length + b.length];
    System.arraycopy(a, 0, res, 0, a.length);
    System.arraycopy(b, 0, res, a.length, b.length);
    return res;
  }

  static <T> T[] _concat(T[] a, T[] b) {
    T[] res = java.util.Arrays.copyOf(a, a.length + b.length);
    System.arraycopy(b, 0, res, a.length, b.length);
    return res;
  }

  static boolean _in(Object item, Object col) {
    if (col instanceof String s && item instanceof String sub) return s.contains(sub);
    if (col instanceof java.util.Map<?, ?> m) return m.containsKey(item);
    if (col != null && col.getClass().isArray()) {
      int n = java.lang.reflect.Array.getLength(col);
      for (int i = 0; i < n; i++) {
        if (java.util.Objects.equals(java.lang.reflect.Array.get(col, i), item)) return true;
      }
      return false;
    }
    if (col instanceof Iterable<?> it) {
      for (Object v : it) {
        if (java.util.Objects.equals(v, item)) return true;
      }
      return false;
    }
    return false;
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
