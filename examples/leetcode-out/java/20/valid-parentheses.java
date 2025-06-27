public class Main {
  static boolean isValid(String s) {
    Object[] stack = new String[] {};
    int n = s.length();
    for (int i = 0; i < n; i++) {
      String c = _indexString(s, i);
      if ((c == "(")) {
        stack = _concat(stack, new String[] {")"});
      } else if ((c == "[")) {
        stack = _concat(stack, new String[] {"]"});
      } else if ((c == "{")) {
        stack = _concat(stack, new String[] {"}"});
      } else {
        if ((stack.length == 0)) {
          return false;
        }
        String top = stack[(stack.length - 1)];
        if ((top != c)) {
          return false;
        }
        stack = _slice(stack, 0, (stack.length - 1));
      }
    }
    return (stack.length == 0);
  }

  static void test_example_1() {
    expect((isValid("()") == true));
  }

  static void test_example_2() {
    expect((isValid("()[]{}") == true));
  }

  static void test_example_3() {
    expect((isValid("(]") == false));
  }

  static void test_example_4() {
    expect((isValid("([)]") == false));
  }

  static void test_example_5() {
    expect((isValid("{[]}") == true));
  }

  static void test_empty_string() {
    expect((isValid("") == true));
  }

  static void test_single_closing() {
    expect((isValid("]") == false));
  }

  static void test_unmatched_open() {
    expect((isValid("((") == false));
  }

  public static void main(String[] args) {
    test_example_1();
    test_example_2();
    test_example_3();
    test_example_4();
    test_example_5();
    test_empty_string();
    test_single_closing();
    test_unmatched_open();
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

  static int[] _slice(int[] arr, int i, int j) {
    if (i < 0) i += arr.length;
    if (j < 0) j += arr.length;
    if (i < 0) i = 0;
    if (j > arr.length) j = arr.length;
    if (j < i) j = i;
    int[] res = new int[j - i];
    System.arraycopy(arr, i, res, 0, j - i);
    return res;
  }

  static double[] _slice(double[] arr, int i, int j) {
    if (i < 0) i += arr.length;
    if (j < 0) j += arr.length;
    if (i < 0) i = 0;
    if (j > arr.length) j = arr.length;
    if (j < i) j = i;
    double[] res = new double[j - i];
    System.arraycopy(arr, i, res, 0, j - i);
    return res;
  }

  static boolean[] _slice(boolean[] arr, int i, int j) {
    if (i < 0) i += arr.length;
    if (j < 0) j += arr.length;
    if (i < 0) i = 0;
    if (j > arr.length) j = arr.length;
    if (j < i) j = i;
    boolean[] res = new boolean[j - i];
    System.arraycopy(arr, i, res, 0, j - i);
    return res;
  }

  static <T> T[] _slice(T[] arr, int i, int j) {
    if (i < 0) i += arr.length;
    if (j < 0) j += arr.length;
    if (i < 0) i = 0;
    if (j > arr.length) j = arr.length;
    if (j < i) j = i;
    return java.util.Arrays.copyOfRange(arr, i, j);
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
