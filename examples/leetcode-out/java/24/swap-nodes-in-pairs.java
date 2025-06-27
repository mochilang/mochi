public class Main {
  static Object[] swapPairs(Object[] nums) {
    int i = 0;
    Object[] result = new Object[] {};
    while ((i < nums.length)) {
      if (((i + 1) < nums.length)) {
        result = _concat(result, new int[] {nums[(i + 1)], nums[i]});
      } else {
        result = _concat(result, new int[] {nums[i]});
      }
      i = (i + 2);
    }
    return result;
  }

  static void test_example_1() {
    expect((swapPairs(new int[] {1, 2, 3, 4}) == new int[] {2, 1, 4, 3}));
  }

  static void test_example_2() {
    expect((swapPairs(new Object[] {}) == new Object[] {}));
  }

  static void test_example_3() {
    expect((swapPairs(new int[] {1}) == new int[] {1}));
  }

  static void test_odd_length() {
    expect((swapPairs(new int[] {1, 2, 3}) == new int[] {2, 1, 3}));
  }

  public static void main(String[] args) {
    test_example_1();
    test_example_2();
    test_example_3();
    test_odd_length();
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
