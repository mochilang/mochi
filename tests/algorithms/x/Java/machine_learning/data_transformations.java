public class Main {

    static double floor(double x) {
        long i = (long)(((Number)(x)).intValue());
        if ((double)((((Number)(i)).doubleValue())) > (double)(x)) {
            i = (long)((long)(i) - 1L);
        }
        return ((Number)(i)).doubleValue();
    }

    static double pow10(long n) {
        double result = (double)(1.0);
        long i_2 = 0L;
        while ((long)(i_2) < (long)(n)) {
            result = (double)((double)(result) * (double)(10.0));
            i_2 = (long)((long)(i_2) + 1L);
        }
        return result;
    }

    static double round(double x, long n) {
        double m = (double)(pow10((long)(n)));
        double y_1 = (double)(((Number)(Math.floor((double)((double)(x) * (double)(m)) + (double)(0.5)))).doubleValue());
        return (double)(y_1) / (double)(m);
    }

    static double sqrtApprox(double x) {
        double guess = (double)(x);
        long i_4 = 0L;
        while ((long)(i_4) < 20L) {
            guess = (double)((double)(((double)(guess) + (double)((double)(x) / (double)(guess)))) / (double)(2.0));
            i_4 = (long)((long)(i_4) + 1L);
        }
        return guess;
    }

    static double mean(double[] data) {
        double total = (double)(0.0);
        long i_6 = 0L;
        long n_1 = (long)(data.length);
        while ((long)(i_6) < (long)(n_1)) {
            total = (double)((double)(total) + (double)(data[(int)((long)(i_6))]));
            i_6 = (long)((long)(i_6) + 1L);
        }
        return (double)(total) / (double)((((Number)(n_1)).doubleValue()));
    }

    static double stdev(double[] data) {
        long n_2 = (long)(data.length);
        if ((long)(n_2) <= 1L) {
            throw new RuntimeException(String.valueOf("data length must be > 1"));
        }
        double m_2 = (double)(mean(((double[])(data))));
        double sum_sq_1 = (double)(0.0);
        long i_8 = 0L;
        while ((long)(i_8) < (long)(n_2)) {
            double diff_1 = (double)((double)(data[(int)((long)(i_8))]) - (double)(m_2));
            sum_sq_1 = (double)((double)(sum_sq_1) + (double)((double)(diff_1) * (double)(diff_1)));
            i_8 = (long)((long)(i_8) + 1L);
        }
        return sqrtApprox((double)((double)(sum_sq_1) / (double)((((Number)(((long)(n_2) - 1L))).doubleValue()))));
    }

    static double[] normalization(double[] data, long ndigits) {
        double x_min = (double)(((Number)(_min(data))).doubleValue());
        double x_max_1 = (double)(((Number)(_max(data))).doubleValue());
        double denom_1 = (double)((double)(x_max_1) - (double)(x_min));
        double[] result_2 = ((double[])(new double[]{}));
        long i_10 = 0L;
        long n_4 = (long)(data.length);
        while ((long)(i_10) < (long)(n_4)) {
            double norm_1 = (double)((double)(((double)(data[(int)((long)(i_10))]) - (double)(x_min))) / (double)(denom_1));
            result_2 = ((double[])(appendDouble(result_2, (double)(round((double)(norm_1), (long)(ndigits))))));
            i_10 = (long)((long)(i_10) + 1L);
        }
        return result_2;
    }

    static double[] standardization(double[] data, long ndigits) {
        double mu = (double)(mean(((double[])(data))));
        double sigma_1 = (double)(stdev(((double[])(data))));
        double[] result_4 = ((double[])(new double[]{}));
        long i_12 = 0L;
        long n_6 = (long)(data.length);
        while ((long)(i_12) < (long)(n_6)) {
            double z_1 = (double)((double)(((double)(data[(int)((long)(i_12))]) - (double)(mu))) / (double)(sigma_1));
            result_4 = ((double[])(appendDouble(result_4, (double)(round((double)(z_1), (long)(ndigits))))));
            i_12 = (long)((long)(i_12) + 1L);
        }
        return result_4;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(normalization(((double[])(new double[]{2.0, 7.0, 10.0, 20.0, 30.0, 50.0})), 3L)));
            System.out.println(_p(normalization(((double[])(new double[]{5.0, 10.0, 15.0, 20.0, 25.0})), 3L)));
            System.out.println(_p(standardization(((double[])(new double[]{2.0, 7.0, 10.0, 20.0, 30.0, 50.0})), 3L)));
            System.out.println(_p(standardization(((double[])(new double[]{5.0, 10.0, 15.0, 20.0, 25.0})), 3L)));
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

    static Object[] _toObjectArray(Object v) {
        if (v instanceof Object[]) return (Object[]) v;
        if (v instanceof int[]) return java.util.Arrays.stream((int[]) v).boxed().toArray();
        if (v instanceof double[]) return java.util.Arrays.stream((double[]) v).boxed().toArray();
        if (v instanceof long[]) return java.util.Arrays.stream((long[]) v).boxed().toArray();
        if (v instanceof boolean[]) { boolean[] a = (boolean[]) v; Object[] out = new Object[a.length]; for (int i = 0; i < a.length; i++) out[i] = a[i]; return out; }
        return (Object[]) v;
    }

    static double _min(Object a) {
        Object[] arr = _toObjectArray(a);
        double m = ((Number)arr[0]).doubleValue();
        for (int i = 1; i < arr.length; i++) {
            double v = ((Number)arr[i]).doubleValue();
            if (v < m) m = v;
        }
        return m;
    }

    static double _max(Object a) {
        Object[] arr = _toObjectArray(a);
        double m = ((Number)arr[0]).doubleValue();
        for (int i = 1; i < arr.length; i++) {
            double v = ((Number)arr[i]).doubleValue();
            if (v > m) m = v;
        }
        return m;
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
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
