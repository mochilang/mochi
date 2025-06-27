public class Main {
  static boolean isMatch(String s, String p) {
    int m = s.length();
    int n = p.length();
    Object[] dp = new Object[][] {};
    int i = 0;
    while ((i <= m)) {
      Object[] row = new boolean[] {};
      int j = 0;
      while ((j <= n)) {
        row = _concat(row, new boolean[] {false});
        j = (j + 1);
      }
      dp = _concat(dp, new Object[][] {row});
      i = (i + 1);
    }
    dp[m][n] = _cast(Object[].class, true);
    int i2 = m;
    while ((i2 >= 0)) {
      int j2 = (n - 1);
      while ((j2 >= 0)) {
        boolean first = false;
        if ((i2 < m)) {
          if (((_indexString(p, j2) == _indexString(s, i2)) || (_indexString(p, j2) == "."))) {
            first = true;
          }
        }
        boolean star = false;
        if (((j2 + 1) < n)) {
          if ((_indexString(p, (j2 + 1)) == "*")) {
            star = true;
          }
        }
        if (star) {
          boolean ok = false;
          if (dp[i2][(j2 + 2)]) {
            ok = true;
          } else {
            if (first) {
              if (dp[(i2 + 1)][j2]) {
                ok = true;
              }
            }
          }
          dp[i2][j2] = _cast(Object[].class, ok);
        } else {
          boolean ok = false;
          if (first) {
            if (dp[(i2 + 1)][(j2 + 1)]) {
              ok = true;
            }
          }
          dp[i2][j2] = _cast(Object[].class, ok);
        }
        j2 = (j2 - 1);
      }
      i2 = (i2 - 1);
    }
    return dp[0][0];
  }

  static void test_example_1() {
    expect((isMatch("aa", "a") == false));
  }

  static void test_example_2() {
    expect((isMatch("aa", "a*") == true));
  }

  static void test_example_3() {
    expect((isMatch("ab", ".*") == true));
  }

  static void test_example_4() {
    expect((isMatch("aab", "c*a*b") == true));
  }

  static void test_example_5() {
    expect((isMatch("mississippi", "mis*is*p*.") == false));
  }

  public static void main(String[] args) {
    test_example_1();
    test_example_2();
    test_example_3();
    test_example_4();
    test_example_5();
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

  static String _indexString(String s, int i) {
    char[] runes = s.toCharArray();
    if (i < 0) i += runes.length;
    if (i < 0 || i >= runes.length) throw new RuntimeException("index out of range");
    return String.valueOf(runes[i]);
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
