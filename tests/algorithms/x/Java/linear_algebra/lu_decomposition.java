public class Main {
    static class LU {
        double[][] lower;
        double[][] upper;
        LU(double[][] lower, double[][] upper) {
            this.lower = lower;
            this.upper = upper;
        }
        LU() {}
        @Override public String toString() {
            return String.format("{'lower': %s, 'upper': %s}", String.valueOf(lower), String.valueOf(upper));
        }
    }

    static double[][] matrix;
    static LU result;

    static LU lu_decomposition(double[][] mat) {
        int n = mat.length;
        if (n == 0) {
            return new LU(new double[][]{}, new double[][]{});
        }
        int m = mat[0].length;
        if (n != m) {
            throw new RuntimeException(String.valueOf("Matrix must be square"));
        }
        double[][] lower = ((double[][])(new double[][]{}));
        double[][] upper = ((double[][])(new double[][]{}));
        int i = 0;
        while (i < n) {
            double[] lrow = ((double[])(new double[]{}));
            double[] urow = ((double[])(new double[]{}));
            int j = 0;
            while (j < n) {
                lrow = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(lrow), java.util.stream.DoubleStream.of(0.0)).toArray()));
                urow = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(urow), java.util.stream.DoubleStream.of(0.0)).toArray()));
                j = j + 1;
            }
            lower = ((double[][])(appendObj(lower, lrow)));
            upper = ((double[][])(appendObj(upper, urow)));
            i = i + 1;
        }
        i = 0;
        while (i < n) {
            int j1 = 0;
            while (j1 < i) {
                double total = 0.0;
                int k = 0;
                while (k < i) {
                    total = total + lower[i][k] * upper[k][j1];
                    k = k + 1;
                }
                if (upper[j1][j1] == 0.0) {
                    throw new RuntimeException(String.valueOf("No LU decomposition exists"));
                }
lower[i][j1] = (mat[i][j1] - total) / upper[j1][j1];
                j1 = j1 + 1;
            }
lower[i][i] = 1.0;
            int j2 = i;
            while (j2 < n) {
                double total2 = 0.0;
                int k2 = 0;
                while (k2 < i) {
                    total2 = total2 + lower[i][k2] * upper[k2][j2];
                    k2 = k2 + 1;
                }
upper[i][j2] = mat[i][j2] - total2;
                j2 = j2 + 1;
            }
            i = i + 1;
        }
        return new LU(lower, upper);
    }

    static void print_matrix(double[][] mat) {
        int i_1 = 0;
        while (i_1 < mat.length) {
            String line = "";
            int j_1 = 0;
            while (j_1 < mat[i_1].length) {
                line = line + _p(_geto(mat[i_1], j_1));
                if (j_1 + 1 < mat[i_1].length) {
                    line = line + " ";
                }
                j_1 = j_1 + 1;
            }
            System.out.println(line);
            i_1 = i_1 + 1;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            matrix = ((double[][])(new double[][]{new double[]{2.0, -2.0, 1.0}, new double[]{0.0, 1.0, 2.0}, new double[]{5.0, 3.0, 1.0}}));
            result = lu_decomposition(((double[][])(matrix)));
            print_matrix(((double[][])(result.lower)));
            print_matrix(((double[][])(result.upper)));
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

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }

    static Object _geto(Object[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
