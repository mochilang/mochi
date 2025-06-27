public class Main {
  static Object[] reverseKGroup(Object[] nums, int k) {
    int n = nums.length;
    if ((k <= 1)) {
      return nums;
    }
    Object[] result = new Object[] {};
    int i = 0;
    while ((i < n)) {
      int end = (i + k);
      if ((end <= n)) {
        int j = (end - 1);
        while ((j >= i)) {
          result = _concat(result, new int[] {nums[j]});
          j = (j - 1);
        }
      } else {
        int j = i;
        while ((j < n)) {
          result = _concat(result, new int[] {nums[j]});
          j = (j + 1);
        }
      }
      i = (i + k);
    }
    return result;
  }

  static void test_example_1() {
    expect((reverseKGroup(new int[] {1, 2, 3, 4, 5}, 2) == new int[] {2, 1, 4, 3, 5}));
  }

  static void test_example_2() {
    expect((reverseKGroup(new int[] {1, 2, 3, 4, 5}, 3) == new int[] {3, 2, 1, 4, 5}));
  }

  static void test_k_equals_list_length() {
    expect((reverseKGroup(new int[] {1, 2, 3, 4}, 4) == new int[] {4, 3, 2, 1}));
  }

  static void test_k_greater_than_length() {
    expect((reverseKGroup(new int[] {1, 2, 3}, 5) == new int[] {1, 2, 3}));
  }

  static void test_k_is_one() {
    expect((reverseKGroup(new int[] {1, 2, 3}, 1) == new int[] {1, 2, 3}));
  }

  public static void main(String[] args) {
    test_example_1();
    test_example_2();
    test_k_equals_list_length();
    test_k_greater_than_length();
    test_k_is_one();
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
