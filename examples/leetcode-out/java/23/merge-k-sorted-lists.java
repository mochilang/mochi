public class Main {
  static Object[] mergeKLists(Object[] lists) {
    int k = lists.length;
    Object[] indices = new int[] {};
    int i = 0;
    while ((i < k)) {
      indices = _concat(indices, new int[] {0});
      i = (i + 1);
    }
    Object[] result = new int[] {};
    while (true) {
      int best = 0;
      int bestList = (-1);
      boolean found = false;
      int j = 0;
      while ((j < k)) {
        int idx = indices[j];
        if ((idx < lists[j].length)) {
          int val = lists[j][idx];
          if (((!found) || (val < best))) {
            best = val;
            bestList = j;
            found = true;
          }
        }
        j = (j + 1);
      }
      if ((!found)) {
        break;
      }
      result = _concat(result, new int[] {best});
      indices[bestList] = _cast(Object[].class, (indices[bestList] + 1));
    }
    return result;
  }

  static void test_example_1() {
    expect(
        (mergeKLists(new Object[][] {new int[] {1, 4, 5}, new int[] {1, 3, 4}, new int[] {2, 6}})
            == new int[] {1, 1, 2, 3, 4, 4, 5, 6}));
  }

  static void test_example_2() {
    expect((mergeKLists(new Object[] {}) == new Object[] {}));
  }

  static void test_example_3() {
    expect((mergeKLists(new Object[][] {new Object[] {}}) == new Object[] {}));
  }

  public static void main(String[] args) {
    test_example_1();
    test_example_2();
    test_example_3();
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
