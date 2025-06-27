public class Main {
  static int maxArea(Object[] height) {
    int left = 0;
    int right = (height.length - 1);
    int maxArea = 0;
    while ((left < right)) {
      int width = (right - left);
      int h = 0;
      if ((height[left] < height[right])) {
        h = height[left];
      } else {
        h = height[right];
      }
      int area = (h * width);
      if ((area > maxArea)) {
        maxArea = area;
      }
      if ((height[left] < height[right])) {
        left = (left + 1);
      } else {
        right = (right - 1);
      }
    }
    return maxArea;
  }

  static void test_example_1() {
    expect((maxArea(new int[] {1, 8, 6, 2, 5, 4, 8, 3, 7}) == 49));
  }

  static void test_example_2() {
    expect((maxArea(new int[] {1, 1}) == 1));
  }

  static void test_decreasing_heights() {
    expect((maxArea(new int[] {4, 3, 2, 1, 4}) == 16));
  }

  static void test_short_array() {
    expect((maxArea(new int[] {1, 2, 1}) == 2));
  }

  public static void main(String[] args) {
    test_example_1();
    test_example_2();
    test_decreasing_heights();
    test_short_array();
  }

  static void expect(boolean cond) {
    if (!cond) throw new RuntimeException("expect failed");
  }
}
