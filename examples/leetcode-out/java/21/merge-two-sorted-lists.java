public class Main {
  static Object[] mergeTwoLists(Object[] l1, Object[] l2) {
    int i = 0;
    int j = 0;
    Object[] result = new Object[] {};
    while (((i < l1.length) && (j < l2.length))) {
      if ((l1[i] <= l2[j])) {
        result = _concat(result, new int[] {l1[i]});
        i = (i + 1);
      } else {
        result = _concat(result, new int[] {l2[j]});
        j = (j + 1);
      }
    }
    while ((i < l1.length)) {
      result = _concat(result, new int[] {l1[i]});
      i = (i + 1);
    }
    while ((j < l2.length)) {
      result = _concat(result, new int[] {l2[j]});
      j = (j + 1);
    }
    return result;
  }

  static void test_example_1() {
    expect(
        (mergeTwoLists(new int[] {1, 2, 4}, new int[] {1, 3, 4}) == new int[] {1, 1, 2, 3, 4, 4}));
  }

  static void test_example_2() {
    expect((mergeTwoLists(new Object[] {}, new Object[] {}) == new Object[] {}));
  }

  static void test_example_3() {
    expect((mergeTwoLists(new Object[] {}, new int[] {0}) == new int[] {0}));
  }

  static void test_different_lengths() {
    expect(
        (mergeTwoLists(new int[] {1, 5, 7}, new int[] {2, 3, 4, 6, 8})
            == new int[] {1, 2, 3, 4, 5, 6, 7, 8}));
  }

  static void test_one_list_empty() {
    expect((mergeTwoLists(new int[] {1, 2, 3}, new Object[] {}) == new int[] {1, 2, 3}));
  }

  public static void main(String[] args) {
    test_example_1();
    test_example_2();
    test_example_3();
    test_different_lengths();
    test_one_list_empty();
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
