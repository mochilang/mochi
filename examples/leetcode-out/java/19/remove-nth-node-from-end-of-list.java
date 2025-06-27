public class Main {
  static Object[] removeNthFromEnd(Object[] nums, int n) {
    int idx = (nums.length - n);
    Object[] result = new Object[] {};
    int i = 0;
    while ((i < nums.length)) {
      if ((i != idx)) {
        result = _concat(result, new int[] {nums[i]});
      }
      i = (i + 1);
    }
    return result;
  }

  static void test_example_1() {
    expect((removeNthFromEnd(new int[] {1, 2, 3, 4, 5}, 2) == new int[] {1, 2, 3, 5}));
  }

  static void test_example_2() {
    expect((removeNthFromEnd(new int[] {1}, 1) == new Object[] {}));
  }

  static void test_example_3() {
    expect((removeNthFromEnd(new int[] {1, 2}, 1) == new int[] {1}));
  }

  static void test_remove_first() {
    expect((removeNthFromEnd(new int[] {7, 8, 9}, 3) == new int[] {8, 9}));
  }

  static void test_remove_last() {
    expect((removeNthFromEnd(new int[] {7, 8, 9}, 1) == new int[] {7, 8}));
  }

  public static void main(String[] args) {
    test_example_1();
    test_example_2();
    test_example_3();
    test_remove_first();
    test_remove_last();
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
