public class Main {
  static int removeElement(Object[] nums, int val) {
    int k = 0;
    int i = 0;
    while ((i < nums.length)) {
      if ((nums[i] != val)) {
        nums[k] = _cast(Object[].class, nums[i]);
        k = (k + 1);
      }
      i = (i + 1);
    }
    return k;
  }

  static void test_example_1() {
    Object[] nums = new int[] {3, 2, 2, 3};
    int k = removeElement(nums, 3);
    expect((k == 2));
    expect((_slice(nums, 0, k) == new int[] {2, 2}));
  }

  static void test_example_2() {
    Object[] nums = new int[] {0, 1, 2, 2, 3, 0, 4, 2};
    int k = removeElement(nums, 2);
    expect((k == 5));
    expect((_slice(nums, 0, k) == new int[] {0, 1, 3, 0, 4}));
  }

  static void test_no_removal() {
    Object[] nums = new int[] {1, 2, 3};
    int k = removeElement(nums, 4);
    expect((k == 3));
    expect((_slice(nums, 0, k) == new int[] {1, 2, 3}));
  }

  public static void main(String[] args) {
    test_example_1();
    test_example_2();
    test_no_removal();
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

  static void expect(boolean cond) {
    if (!cond) throw new RuntimeException("expect failed");
  }

  static <T> T _cast(Class<T> cls, Object v) {
    if (cls.isInstance(v)) return cls.cast(v);
    if (cls == Integer.class) {
      if (v instanceof Number n) return cls.cast(n.intValue());
      if (v instanceof String s) return cls.cast(Integer.parseInt(s));
      return cls.cast(0);
    }
    if (cls == Double.class) {
      if (v instanceof Number n) return cls.cast(n.doubleValue());
      if (v instanceof String s) return cls.cast(Double.parseDouble(s));
      return cls.cast(0.0);
    }
    if (cls == Boolean.class) {
      if (v instanceof Boolean b) return cls.cast(b);
      if (v instanceof String s) return cls.cast(Boolean.parseBoolean(s));
      return cls.cast(false);
    }
    if (v instanceof java.util.Map<?, ?> m) {
      try {
        T out = cls.getDeclaredConstructor().newInstance();
        for (java.lang.reflect.Field f : cls.getDeclaredFields()) {
          Object val = m.get(f.getName());
          if (val != null) {
            f.setAccessible(true);
            Class<?> ft = f.getType();
            if (ft == int.class) {
              if (val instanceof Number n) f.setInt(out, n.intValue());
              else if (val instanceof String s) f.setInt(out, Integer.parseInt(s));
            } else if (ft == double.class) {
              if (val instanceof Number n) f.setDouble(out, n.doubleValue());
              else if (val instanceof String s) f.setDouble(out, Double.parseDouble(s));
            } else if (ft == boolean.class) {
              if (val instanceof Boolean b) f.setBoolean(out, b);
              else if (val instanceof String s) f.setBoolean(out, Boolean.parseBoolean(s));
            } else {
              f.set(out, val);
            }
          }
        }
        return out;
      } catch (Exception e) {
        throw new RuntimeException(e);
      }
    }
    try {
      return cls.getDeclaredConstructor().newInstance();
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }
}
