public class Main {

    static double exp_approx(double x) {
        double sum = 1.0;
        double term_1 = 1.0;
        long i_1 = 1L;
        double absx_1 = (double)((double)(x) < 0.0 ? -x : x);
        while ((long)(i_1) <= (long)(20)) {
            term_1 = (double)(term_1) * absx_1 / (((Number)(i_1)).doubleValue());
            sum = (double)(sum) + (double)(term_1);
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        if ((double)(x) < 0.0) {
            return 1.0 / (double)(sum);
        }
        return sum;
    }

    static double[] exponential_linear_unit(double[] vector, double alpha) {
        double[] result = ((double[])(new double[]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(vector.length)) {
            double v_1 = (double)(vector[(int)((long)(i_3))]);
            if ((double)(v_1) > 0.0) {
                result = ((double[])(appendDouble(result, (double)(v_1))));
            } else {
                double neg_1 = (double)(alpha) * ((double)(exp_approx((double)(v_1))) - 1.0);
                result = ((double[])(appendDouble(result, neg_1)));
            }
            i_3 = (long)((long)(i_3) + (long)(1));
        }
        return result;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(exponential_linear_unit(((double[])(new double[]{2.3, 0.6, -2.0, -3.8})), 0.3)));
            System.out.println(_p(exponential_linear_unit(((double[])(new double[]{-9.2, -0.3, 0.45, -4.56})), 0.067)));
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
}
