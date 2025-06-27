public class Main {
  static int[] addTwoNumbers(int[] l1, int[] l2) {
    int i = 0;
    int j = 0;
    int carry = 0;
    int[] result = new int[] {};
    while ((((i < l1.length) || (j < l2.length)) || (carry > 0))) {
      int x = 0;
      if ((i < l1.length)) {
        x = l1[i];
        i = (i + 1);
      }
      int y = 0;
      if ((j < l2.length)) {
        y = l2[j];
        j = (j + 1);
      }
      int sum = ((x + y) + carry);
      int digit = (sum % 10);
      carry = (sum / 10);
      result = _concat(result, new int[] {digit});
    }
    return result;
  }

  public static void main(String[] args) {}

  static int[] _concat(int[] a, int[] b) {
    int[] res = new int[a.length + b.length];
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
}
