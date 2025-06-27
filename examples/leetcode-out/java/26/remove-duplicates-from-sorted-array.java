public class Main {
  static int removeDuplicates(Object[] nums) {
    if ((nums.length == 0)) {
      return 0;
    }
    int count = 1;
    int prev = nums[0];
    int i = 1;
    while ((i < nums.length)) {
      int cur = nums[i];
      if ((cur != prev)) {
        count = (count + 1);
        prev = cur;
      }
      i = (i + 1);
    }
    return count;
  }

  static void test_example_1() {
    expect((removeDuplicates(new int[] {1, 1, 2}) == 2));
  }

  static void test_example_2() {
    expect((removeDuplicates(new int[] {0, 0, 1, 1, 1, 2, 2, 3, 3, 4}) == 5));
  }

  static void test_empty() {
    expect((removeDuplicates(new Object[] {}) == 0));
  }

  public static void main(String[] args) {
    test_example_1();
    test_example_2();
    test_empty();
  }

  static void expect(boolean cond) {
    if (!cond) throw new RuntimeException("expect failed");
  }
}
