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
        long n = mat.length;
        if (n == 0) {
            return new LU(new double[][]{}, new double[][]{});
        }
        long m_1 = mat[(int)((long)(0))].length;
        if (n != m_1) {
            throw new RuntimeException(String.valueOf("Matrix must be square"));
        }
        double[][] lower_1 = ((double[][])(new double[][]{}));
        double[][] upper_1 = ((double[][])(new double[][]{}));
        long i_1 = 0L;
        while (i_1 < n) {
            double[] lrow_1 = ((double[])(new double[]{}));
            double[] urow_1 = ((double[])(new double[]{}));
            long j_1 = 0L;
            while (j_1 < n) {
                lrow_1 = ((double[])(appendDouble(lrow_1, 0.0)));
                urow_1 = ((double[])(appendDouble(urow_1, 0.0)));
                j_1 = j_1 + 1;
            }
            lower_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(lower_1), java.util.stream.Stream.of(lrow_1)).toArray(double[][]::new)));
            upper_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(upper_1), java.util.stream.Stream.of(urow_1)).toArray(double[][]::new)));
            i_1 = i_1 + 1;
        }
        i_1 = 0L;
        while (i_1 < n) {
            long j1_1 = 0L;
            while (j1_1 < i_1) {
                double total_1 = 0.0;
                long k_1 = 0L;
                while (k_1 < i_1) {
                    total_1 = total_1 + lower_1[(int)((long)(i_1))][(int)((long)(k_1))] * upper_1[(int)((long)(k_1))][(int)((long)(j1_1))];
                    k_1 = k_1 + 1;
                }
                if (upper_1[(int)((long)(j1_1))][(int)((long)(j1_1))] == 0.0) {
                    throw new RuntimeException(String.valueOf("No LU decomposition exists"));
                }
lower_1[(int)((long)(i_1))][(int)((long)(j1_1))] = (mat[(int)((long)(i_1))][(int)((long)(j1_1))] - total_1) / upper_1[(int)((long)(j1_1))][(int)((long)(j1_1))];
                j1_1 = j1_1 + 1;
            }
lower_1[(int)((long)(i_1))][(int)((long)(i_1))] = 1.0;
            long j2_1 = i_1;
            while (j2_1 < n) {
                double total2_1 = 0.0;
                long k2_1 = 0L;
                while (k2_1 < i_1) {
                    total2_1 = total2_1 + lower_1[(int)((long)(i_1))][(int)((long)(k2_1))] * upper_1[(int)((long)(k2_1))][(int)((long)(j2_1))];
                    k2_1 = k2_1 + 1;
                }
upper_1[(int)((long)(i_1))][(int)((long)(j2_1))] = mat[(int)((long)(i_1))][(int)((long)(j2_1))] - total2_1;
                j2_1 = j2_1 + 1;
            }
            i_1 = i_1 + 1;
        }
        return new LU(lower_1, upper_1);
    }

    static void print_matrix(double[][] mat) {
        long i_2 = 0L;
        while (i_2 < mat.length) {
            String line_1 = "";
            long j_3 = 0L;
            while (j_3 < mat[(int)((long)(i_2))].length) {
                line_1 = line_1 + _p(_getd(mat[(int)((long)(i_2))], ((Number)(j_3)).intValue()));
                if (j_3 + 1 < mat[(int)((long)(i_2))].length) {
                    line_1 = line_1 + " ";
                }
                j_3 = j_3 + 1;
            }
            System.out.println(line_1);
            i_2 = i_2 + 1;
        }
    }
    public static void main(String[] args) {
        matrix = ((double[][])(new double[][]{new double[]{2.0, -2.0, 1.0}, new double[]{0.0, 1.0, 2.0}, new double[]{5.0, 3.0, 1.0}}));
        result = lu_decomposition(((double[][])(matrix)));
        print_matrix(((double[][])(result.lower)));
        print_matrix(((double[][])(result.upper)));
    }

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static Double _getd(double[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
