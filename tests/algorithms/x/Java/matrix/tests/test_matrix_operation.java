public class Main {

    static void check_matrix(double[][] mat) {
        if ((long)(mat.length) < (long)(2) || (long)(((double[])_geto(mat, (int)((long)(0)))).length) < (long)(2)) {
            throw new RuntimeException(String.valueOf("Expected a matrix with at least 2x2 dimensions"));
        }
    }

    static double[][] add(double[][] a, double[][] b) {
        check_matrix(((double[][])(a)));
        check_matrix(((double[][])(b)));
        if ((long)(a.length) != (long)(b.length) || (long)(((double[])_geto(a, (int)((long)(0)))).length) != (long)(((double[])_geto(b, (int)((long)(0)))).length)) {
            throw new RuntimeException(String.valueOf("Matrices must have the same dimensions"));
        }
        long rows_1 = (long)(a.length);
        long cols_1 = (long)(((double[])_geto(a, (int)((long)(0)))).length);
        double[][] result_1 = ((double[][])(new double[][]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(rows_1)) {
            double[] row_1 = ((double[])(new double[]{}));
            long j_1 = 0L;
            while ((long)(j_1) < (long)(cols_1)) {
                row_1 = ((double[])(appendDouble(row_1, (double)(_getd(((double[])_geto(a, (int)((long)(i_1)))), (int)((long)(j_1)))) + (double)(_getd(((double[])_geto(b, (int)((long)(i_1)))), (int)((long)(j_1)))))));
                j_1 = (long)((long)(j_1) + (long)(1));
            }
            result_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(row_1)).toArray(double[][]::new)));
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        return result_1;
    }

    static double[][] subtract(double[][] a, double[][] b) {
        check_matrix(((double[][])(a)));
        check_matrix(((double[][])(b)));
        if ((long)(a.length) != (long)(b.length) || (long)(((double[])_geto(a, (int)((long)(0)))).length) != (long)(((double[])_geto(b, (int)((long)(0)))).length)) {
            throw new RuntimeException(String.valueOf("Matrices must have the same dimensions"));
        }
        long rows_3 = (long)(a.length);
        long cols_3 = (long)(((double[])_geto(a, (int)((long)(0)))).length);
        double[][] result_3 = ((double[][])(new double[][]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(rows_3)) {
            double[] row_3 = ((double[])(new double[]{}));
            long j_3 = 0L;
            while ((long)(j_3) < (long)(cols_3)) {
                row_3 = ((double[])(appendDouble(row_3, (double)(_getd(((double[])_geto(a, (int)((long)(i_3)))), (int)((long)(j_3)))) - (double)(_getd(((double[])_geto(b, (int)((long)(i_3)))), (int)((long)(j_3)))))));
                j_3 = (long)((long)(j_3) + (long)(1));
            }
            result_3 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_3), java.util.stream.Stream.of(row_3)).toArray(double[][]::new)));
            i_3 = (long)((long)(i_3) + (long)(1));
        }
        return result_3;
    }

    static double[][] scalar_multiply(double[][] a, double s) {
        check_matrix(((double[][])(a)));
        long rows_5 = (long)(a.length);
        long cols_5 = (long)(((double[])_geto(a, (int)((long)(0)))).length);
        double[][] result_5 = ((double[][])(new double[][]{}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(rows_5)) {
            double[] row_5 = ((double[])(new double[]{}));
            long j_5 = 0L;
            while ((long)(j_5) < (long)(cols_5)) {
                row_5 = ((double[])(appendDouble(row_5, (double)(_getd(((double[])_geto(a, (int)((long)(i_5)))), (int)((long)(j_5)))) * (double)(s))));
                j_5 = (long)((long)(j_5) + (long)(1));
            }
            result_5 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_5), java.util.stream.Stream.of(row_5)).toArray(double[][]::new)));
            i_5 = (long)((long)(i_5) + (long)(1));
        }
        return result_5;
    }

    static double[][] multiply(double[][] a, double[][] b) {
        check_matrix(((double[][])(a)));
        check_matrix(((double[][])(b)));
        if ((long)(((double[])_geto(a, (int)((long)(0)))).length) != (long)(b.length)) {
            throw new RuntimeException(String.valueOf("Invalid dimensions for matrix multiplication"));
        }
        long rows_7 = (long)(a.length);
        long cols_7 = (long)(((double[])_geto(b, (int)((long)(0)))).length);
        double[][] result_7 = ((double[][])(new double[][]{}));
        long i_7 = 0L;
        while ((long)(i_7) < (long)(rows_7)) {
            double[] row_7 = ((double[])(new double[]{}));
            long j_7 = 0L;
            while ((long)(j_7) < (long)(cols_7)) {
                double sum_1 = 0.0;
                long k_1 = 0L;
                while ((long)(k_1) < (long)(b.length)) {
                    sum_1 = sum_1 + (double)(_getd(((double[])_geto(a, (int)((long)(i_7)))), (int)((long)(k_1)))) * (double)(_getd(((double[])_geto(b, (int)((long)(k_1)))), (int)((long)(j_7))));
                    k_1 = (long)((long)(k_1) + (long)(1));
                }
                row_7 = ((double[])(appendDouble(row_7, sum_1)));
                j_7 = (long)((long)(j_7) + (long)(1));
            }
            result_7 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_7), java.util.stream.Stream.of(row_7)).toArray(double[][]::new)));
            i_7 = (long)((long)(i_7) + (long)(1));
        }
        return result_7;
    }

    static double[][] identity(long n) {
        double[][] result_8 = ((double[][])(new double[][]{}));
        long i_9 = 0L;
        while ((long)(i_9) < n) {
            double[] row_9 = ((double[])(new double[]{}));
            long j_9 = 0L;
            while ((long)(j_9) < n) {
                if ((long)(i_9) == (long)(j_9)) {
                    row_9 = ((double[])(appendDouble(row_9, 1.0)));
                } else {
                    row_9 = ((double[])(appendDouble(row_9, 0.0)));
                }
                j_9 = (long)((long)(j_9) + (long)(1));
            }
            result_8 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_8), java.util.stream.Stream.of(row_9)).toArray(double[][]::new)));
            i_9 = (long)((long)(i_9) + (long)(1));
        }
        return result_8;
    }

    static double[][] transpose(double[][] a) {
        check_matrix(((double[][])(a)));
        long rows_9 = (long)(a.length);
        long cols_9 = (long)(((double[])_geto(a, (int)((long)(0)))).length);
        double[][] result_10 = ((double[][])(new double[][]{}));
        long j_11 = 0L;
        while ((long)(j_11) < (long)(cols_9)) {
            double[] row_11 = ((double[])(new double[]{}));
            long i_11 = 0L;
            while ((long)(i_11) < (long)(rows_9)) {
                row_11 = ((double[])(appendDouble(row_11, (double)(_getd(((double[])_geto(a, (int)((long)(i_11)))), (int)((long)(j_11)))))));
                i_11 = (long)((long)(i_11) + (long)(1));
            }
            result_10 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_10), java.util.stream.Stream.of(row_11)).toArray(double[][]::new)));
            j_11 = (long)((long)(j_11) + (long)(1));
        }
        return result_10;
    }

    static void main() {
        double[][] mat_a = ((double[][])(new double[][]{new double[]{12.0, 10.0}, new double[]{3.0, 9.0}}));
        double[][] mat_b_1 = ((double[][])(new double[][]{new double[]{3.0, 4.0}, new double[]{7.0, 4.0}}));
        double[][] mat_c_1 = ((double[][])(new double[][]{new double[]{3.0, 0.0, 2.0}, new double[]{2.0, 0.0, -2.0}, new double[]{0.0, 1.0, 1.0}}));
        System.out.println(_p(add(((double[][])(mat_a)), ((double[][])(mat_b_1)))));
        System.out.println(_p(subtract(((double[][])(mat_a)), ((double[][])(mat_b_1)))));
        System.out.println(_p(multiply(((double[][])(mat_a)), ((double[][])(mat_b_1)))));
        System.out.println(_p(scalar_multiply(((double[][])(mat_a)), 3.5)));
        System.out.println(_p(identity(5L)));
        System.out.println(_p(transpose(((double[][])(mat_c_1)))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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

    static double _getd(double[] a, int i) {
        if (a == null) return 0.0;
        if (i < 0) i += a.length;
        if (i < 0 || i >= a.length) return 0.0;
        return a[i];
    }

    static Object _geto(Object[] a, int i) {
        if (a == null) return null;
        if (i < 0) i += a.length;
        if (i < 0 || i >= a.length) return null;
        return a[i];
    }
}
