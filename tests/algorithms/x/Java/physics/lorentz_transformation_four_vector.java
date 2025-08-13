public class Main {
    static double c = 299792458.0;
    static double[] v;

    static double sqrtApprox(double x) {
        if ((double)(x) <= 0.0) {
            return 0.0;
        }
        double guess_1 = (double)(x) / 2.0;
        long i_1 = 0L;
        while ((long)(i_1) < (long)(20)) {
            guess_1 = (guess_1 + (double)(x) / guess_1) / 2.0;
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        return guess_1;
    }

    static double beta(double velocity) {
        if ((double)(velocity) > c) {
            throw new RuntimeException(String.valueOf("Speed must not exceed light speed 299,792,458 [m/s]!"));
        }
        if ((double)(velocity) < 1.0) {
            throw new RuntimeException(String.valueOf("Speed must be greater than or equal to 1!"));
        }
        return (double)(velocity) / c;
    }

    static double gamma(double velocity) {
        double b = (double)(beta((double)(velocity)));
        return 1.0 / (double)(sqrtApprox(1.0 - (double)(b) * (double)(b)));
    }

    static double[][] transformation_matrix(double velocity) {
        double g = (double)(gamma((double)(velocity)));
        double b_2 = (double)(beta((double)(velocity)));
        return new double[][]{new double[]{g, (double)(-g) * (double)(b_2), 0.0, 0.0}, new double[]{(double)(-g) * (double)(b_2), g, 0.0, 0.0}, new double[]{0.0, 0.0, 1.0, 0.0}, new double[]{0.0, 0.0, 0.0, 1.0}};
    }

    static double[] mat_vec_mul(double[][] mat, double[] vec) {
        double[] res = ((double[])(new double[]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(4)) {
            double[] row_1 = ((double[])(mat[(int)((long)(i_3))]));
            double value_1 = (double)(row_1[(int)((long)(0))]) * (double)(vec[(int)((long)(0))]) + (double)(row_1[(int)((long)(1))]) * (double)(vec[(int)((long)(1))]) + (double)(row_1[(int)((long)(2))]) * (double)(vec[(int)((long)(2))]) + (double)(row_1[(int)((long)(3))]) * (double)(vec[(int)((long)(3))]);
            res = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.Arrays.stream(new double[]{value_1})).toArray()));
            i_3 = (long)((long)(i_3) + (long)(1));
        }
        return res;
    }

    static double[] transform(double velocity, double[] event) {
        double g_1 = (double)(gamma((double)(velocity)));
        double b_4 = (double)(beta((double)(velocity)));
        double ct_1 = (double)(event[(int)((long)(0))]) * c;
        double x_1 = (double)(event[(int)((long)(1))]);
        return new double[]{(double)(g_1) * ct_1 - (double)(g_1) * (double)(b_4) * (double)(x_1), (double)(-g_1) * (double)(b_4) * ct_1 + (double)(g_1) * (double)(x_1), event[(int)((long)(2))], event[(int)((long)(3))]};
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(beta(c)));
            System.out.println(_p(beta(199792458.0)));
            System.out.println(_p(beta(100000.0)));
            System.out.println(_p(gamma(4.0)));
            System.out.println(_p(gamma(100000.0)));
            System.out.println(_p(gamma(30000000.0)));
            System.out.println(_p(transformation_matrix(29979245.0)));
            v = ((double[])(transform(29979245.0, ((double[])(new double[]{1.0, 2.0, 3.0, 4.0})))));
            System.out.println(_p(v));
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
}
