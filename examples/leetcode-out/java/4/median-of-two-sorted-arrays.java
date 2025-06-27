public class Main {
  static double findMedianSortedArrays(Object[] nums1, Object[] nums2) {
    Object[] merged = new int[] {};
    int i = 0;
    int j = 0;
    while (((i < nums1.length) || (j < nums2.length))) {
      if ((j >= nums2.length)) {
        merged = _concat(merged, new int[] {nums1[i]});
        i = (i + 1);
      } else if ((i >= nums1.length)) {
        merged = _concat(merged, new int[] {nums2[j]});
        j = (j + 1);
      } else if ((nums1[i] <= nums2[j])) {
        merged = _concat(merged, new int[] {nums1[i]});
        i = (i + 1);
      } else {
        merged = _concat(merged, new int[] {nums2[j]});
        j = (j + 1);
      }
    }
    int total = merged.length;
    if (((total % 2) == 1)) {
      return ((double) _cast(Double.class, merged[(total / 2)]));
    }
    int mid1 = merged[((total / 2) - 1)];
    int mid2 = merged[(total / 2)];
    return (((double) _cast(Double.class, (mid1 + mid2))) / 2);
  }

  static void test_example_1() {
    expect((findMedianSortedArrays(new int[] {1, 3}, new int[] {2}) == 2));
  }

  static void test_example_2() {
    expect((findMedianSortedArrays(new int[] {1, 2}, new int[] {3, 4}) == 2.5));
  }

  static void test_empty_first() {
    expect((findMedianSortedArrays((Object[]) (new Object[] {}), new int[] {1}) == 1));
  }

  static void test_empty_second() {
    expect((findMedianSortedArrays(new int[] {2}, (Object[]) (new Object[] {})) == 2));
  }

  public static void main(String[] args) {
    test_example_1();
    test_example_2();
    test_empty_first();
    test_empty_second();
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
