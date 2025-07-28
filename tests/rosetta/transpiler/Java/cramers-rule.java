public class Main {
    static double[][] m = new double[][]{new double[]{2.0, -1.0, 5.0, 1.0}, new double[]{3.0, 2.0, 2.0, -6.0}, new double[]{1.0, 3.0, 3.0, -1.0}, new double[]{5.0, -2.0, -3.0, 3.0}};
    static double[] v = new double[]{-3.0, -32.0, -47.0, 49.0};
    static double d = det(m);
    static double[] x = new double[]{};
    static int i = 0;
    static String s = "[";
    static int j = 0;

    static double det(double[][] m) {
        int n = m.length;
        if (n == 1) {
            return m[0][0];
        }
        double total = 0.0;
        double sign = 1.0;
        int c = 0;
        while (c < n) {
            double[][] sub = new double[][]{};
            int r = 1;
            while (r < n) {
                double[] row = new double[]{};
                int cc = 0;
                while (cc < n) {
                    if (cc != c) {
                        row = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(m[r][cc])).toArray();
                    }
                    cc = cc + 1;
                }
                sub = appendObj(sub, row);
                r = r + 1;
            }
            total = total + sign * m[0][c] * det(sub);
            sign = sign * (-1.0);
            c = c + 1;
        }
        return total;
    }

    static double[][] replaceCol(double[][] m, int col, double[] v) {
        double[][] res = new double[][]{};
        int r = 0;
        while (r < m.length) {
            double[] row = new double[]{};
            int c = 0;
            while (c < m[r].length) {
                if (c == col) {
                    row = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(v[r])).toArray();
                } else {
                    row = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(m[r][c])).toArray();
                }
                c = c + 1;
            }
            res = appendObj(res, row);
            r = r + 1;
        }
        return res;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            while (i < v.length) {
                double[][] mc = replaceCol(m, i, v);
                x = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(x), java.util.stream.DoubleStream.of(det(mc) / d)).toArray();
                i = i + 1;
            }
            while (j < x.length) {
                s = s + String.valueOf(x[j]);
                if (j < x.length - 1) {
                    s = s + " ";
                }
                j = j + 1;
            }
            s = s + "]";
            System.out.println(s);
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
