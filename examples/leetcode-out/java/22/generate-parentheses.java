public class Main {
  static Object[] generateParenthesis(int n) {
    Object[] result = new String[] {};
    java.util.function.Function backtrack =
        (current, open, close) -> {
          if ((current.length() == (n * 2))) {
            result = _concat(result, new String[] {current});
          } else {
            if ((open < n)) {
              backtrack.apply((current + "("), (open + 1), close);
            }
            if ((close < open)) {
              backtrack.apply((current + ")"), open, (close + 1));
            }
          }
        };
    backtrack.apply("", 0, 0);
    return result;
  }

  static void test_example_1() {
    expect(
        (generateParenthesis(3)
            == new String[] {"((()))", "(()())", "(())()", "()(())", "()()()"}));
  }

  static void test_example_2() {
    expect((generateParenthesis(1) == new String[] {"()"}));
  }

  static void test_two_pairs() {
    expect((generateParenthesis(2) == new String[] {"(())", "()()"}));
  }

  public static void main(String[] args) {
    test_example_1();
    test_example_2();
    test_two_pairs();
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

  static void expect(boolean cond) {
    if (!cond) throw new RuntimeException("expect failed");
  }
}
