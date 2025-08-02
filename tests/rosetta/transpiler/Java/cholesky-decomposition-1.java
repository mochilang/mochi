public class Main {

    static double sqrtApprox(double x) {
        double guess = x;
        int i = 0;
        while (i < 20) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static java.util.Map<String,Object> makeSym(int order, double[] elements) {
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("order", order), java.util.Map.entry("ele", elements)));
    }

    static double[][] unpackSym(java.util.Map<String,Object> m) {
        Object n = (Object)(((Object)(m).get("order")));
        Object ele = (Object)(((Object)(m).get("ele")));
        double[][] mat = ((double[][])(new double[][]{}));
        int idx = 0;
        int r = 0;
        while (r < ((Number)(n)).intValue()) {
            double[] row = ((double[])(new double[]{}));
            int c = 0;
            while (c <= r) {
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(((Number)(ele[idx])).doubleValue())).toArray()));
                idx = idx + 1;
                c = c + 1;
            }
            while (c < ((Number)(n)).intValue()) {
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(0.0)).toArray()));
                c = c + 1;
            }
            mat = ((double[][])(appendObj(mat, row)));
            r = r + 1;
        }
        r = 0;
        while (r < ((Number)(n)).intValue()) {
            int c_1 = r + 1;
            while (c_1 < ((Number)(n)).intValue()) {
mat[r][c_1] = mat[c_1][r];
                c_1 = c_1 + 1;
            }
            r = r + 1;
        }
        return mat;
    }

    static void printMat(double[][] m) {
        int i_1 = 0;
        while (i_1 < m.length) {
            String line = "";
            int j = 0;
            while (j < m[i_1].length) {
                line = line + _p(_geto(m[i_1], j));
                if (j < m[i_1].length - 1) {
                    line = line + " ";
                }
                j = j + 1;
            }
            System.out.println(line);
            i_1 = i_1 + 1;
        }
    }

    static void printSym(java.util.Map<String,Object> m) {
        printMat(((double[][])(unpackSym(m))));
    }

    static void printLower(java.util.Map<String,Object> m) {
        Object n_1 = (Object)(((Object)(m).get("order")));
        Object ele_1 = (Object)(((Object)(m).get("ele")));
        double[][] mat_1 = ((double[][])(new double[][]{}));
        int idx_1 = 0;
        int r_1 = 0;
        while (r_1 < ((Number)(n_1)).intValue()) {
            double[] row_1 = ((double[])(new double[]{}));
            int c_2 = 0;
            while (c_2 <= r_1) {
                row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of(((Number)(ele_1[idx_1])).doubleValue())).toArray()));
                idx_1 = idx_1 + 1;
                c_2 = c_2 + 1;
            }
            while (c_2 < ((Number)(n_1)).intValue()) {
                row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of(0.0)).toArray()));
                c_2 = c_2 + 1;
            }
            mat_1 = ((double[][])(appendObj(mat_1, row_1)));
            r_1 = r_1 + 1;
        }
        printMat(((double[][])(mat_1)));
    }

    static java.util.Map<String,Object> choleskyLower(java.util.Map<String,Object> a) {
        Object n_2 = (Object)(((Object)(a).get("order")));
        Object ae = (Object)(((Object)(a).get("ele")));
        double[] le = ((double[])(new double[]{}));
        int idx_2 = 0;
        while (idx_2 < String.valueOf(ae).length()) {
            le = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(le), java.util.stream.DoubleStream.of(0.0)).toArray()));
            idx_2 = idx_2 + 1;
        }
        int row_2 = 1;
        int col = 1;
        int dr = 0;
        int dc = 0;
        int i_2 = 0;
        while (i_2 < String.valueOf(ae).length()) {
            Object e = ae[i_2];
            if (i_2 < dr) {
                double d = (((Number)(e)).intValue() - le[i_2]) / le[dc];
le[i_2] = d;
                int ci = col;
                int cx = dc;
                int j_1 = i_2 + 1;
                while (j_1 <= dr) {
                    cx = cx + ci;
                    ci = ci + 1;
le[j_1] = le[j_1] + d * le[cx];
                    j_1 = j_1 + 1;
                }
                col = col + 1;
                dc = dc + col;
            } else {
le[i_2] = sqrtApprox(((Number)(e)).intValue() - le[i_2]);
                row_2 = row_2 + 1;
                dr = dr + row_2;
                col = 1;
                dc = 0;
            }
            i_2 = i_2 + 1;
        }
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("order", n_2), java.util.Map.entry("ele", le)));
    }

    static void demo(java.util.Map<String,Object> a) {
        System.out.println("A:");
        printSym(a);
        System.out.println("L:");
        java.util.Map<String,Object> l = choleskyLower(a);
        printLower(l);
    }
    public static void main(String[] args) {
        demo(makeSym(3, ((double[])(new double[]{25.0, 15.0, 18.0, -5.0, 0.0, 11.0}))));
        demo(makeSym(4, ((double[])(new double[]{18.0, 22.0, 70.0, 54.0, 86.0, 174.0, 42.0, 62.0, 134.0, 106.0}))));
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }

    static Object _geto(Object[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
