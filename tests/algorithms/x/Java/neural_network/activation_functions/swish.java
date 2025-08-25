public class Main {

    static double exp_approx(double x) {
        double sum = (double)(1.0);
        double term_1 = (double)(1.0);
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(1);
        while (i_1.compareTo(java.math.BigInteger.valueOf(20)) <= 0) {
            term_1 = (double)((double)((double)(term_1) * (double)(x)) / (double)((((Number)(i_1)).doubleValue())));
            sum = (double)((double)(sum) + (double)(term_1));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return (double)(sum);
    }

    static double[] sigmoid(double[] vector) {
        double[] result = ((double[])(new double[]{}));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(vector.length))) < 0) {
            double v_1 = (double)(vector[_idx((vector).length, ((java.math.BigInteger)(i_3)).longValue())]);
            double s_1 = (double)((double)(1.0) / (double)(((double)(1.0) + (double)(exp_approx((double)(-v_1))))));
            result = ((double[])(appendDouble(result, (double)(s_1))));
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[])(result));
    }

    static double[] swish(double[] vector, double beta) {
        double[] result_1 = ((double[])(new double[]{}));
        java.math.BigInteger i_5 = java.math.BigInteger.valueOf(0);
        while (i_5.compareTo(new java.math.BigInteger(String.valueOf(vector.length))) < 0) {
            double v_3 = (double)(vector[_idx((vector).length, ((java.math.BigInteger)(i_5)).longValue())]);
            double s_3 = (double)((double)(1.0) / (double)(((double)(1.0) + (double)(exp_approx((double)((double)(-beta) * (double)(v_3)))))));
            result_1 = ((double[])(appendDouble(result_1, (double)((double)(v_3) * (double)(s_3)))));
            i_5 = new java.math.BigInteger(String.valueOf(i_5.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[])(result_1));
    }

    static double[] sigmoid_linear_unit(double[] vector) {
        return ((double[])(swish(((double[])(vector)), (double)(1.0))));
    }

    static boolean approx_equal(double a, double b, double eps) {
        double diff = (double)((double)(a) > (double)(b) ? (double)(a) - (double)(b) : (double)(b) - (double)(a));
        return (double)(diff) < (double)(eps);
    }

    static boolean approx_equal_list(double[] a, double[] b, double eps) {
        if (new java.math.BigInteger(String.valueOf(a.length)).compareTo(new java.math.BigInteger(String.valueOf(b.length))) != 0) {
            return false;
        }
        java.math.BigInteger i_7 = java.math.BigInteger.valueOf(0);
        while (i_7.compareTo(new java.math.BigInteger(String.valueOf(a.length))) < 0) {
            if (!(Boolean)approx_equal((double)(a[_idx((a).length, ((java.math.BigInteger)(i_7)).longValue())]), (double)(b[_idx((b).length, ((java.math.BigInteger)(i_7)).longValue())]), (double)(eps))) {
                return false;
            }
            i_7 = new java.math.BigInteger(String.valueOf(i_7.add(java.math.BigInteger.valueOf(1))));
        }
        return true;
    }

    static void test_swish() {
        double[] v_4 = ((double[])(new double[]{(double)(-1.0), (double)(1.0), (double)(2.0)}));
        double eps_1 = (double)(0.001);
        if (!(Boolean)approx_equal_list(((double[])(sigmoid(((double[])(v_4))))), ((double[])(new double[]{(double)(0.26894142), (double)(0.73105858), (double)(0.88079708)})), (double)(eps_1))) {
            throw new RuntimeException(String.valueOf("sigmoid incorrect"));
        }
        if (!(Boolean)approx_equal_list(((double[])(sigmoid_linear_unit(((double[])(v_4))))), ((double[])(new double[]{(double)(-0.26894142), (double)(0.73105858), (double)(1.76159416)})), (double)(eps_1))) {
            throw new RuntimeException(String.valueOf("sigmoid_linear_unit incorrect"));
        }
        if (!(Boolean)approx_equal_list(((double[])(swish(((double[])(v_4)), (double)(2.0)))), ((double[])(new double[]{(double)(-0.11920292), (double)(0.88079708), (double)(1.96402758)})), (double)(eps_1))) {
            throw new RuntimeException(String.valueOf("swish incorrect"));
        }
        if (!(Boolean)approx_equal_list(((double[])(swish(((double[])(new double[]{(double)(-2.0)})), (double)(1.0)))), ((double[])(new double[]{(double)(-0.23840584)})), (double)(eps_1))) {
            throw new RuntimeException(String.valueOf("swish with parameter 1 incorrect"));
        }
    }

    static void main() {
        test_swish();
        System.out.println(_p(sigmoid(((double[])(new double[]{(double)(-1.0), (double)(1.0), (double)(2.0)})))));
        System.out.println(_p(sigmoid_linear_unit(((double[])(new double[]{(double)(-1.0), (double)(1.0), (double)(2.0)})))));
        System.out.println(_p(swish(((double[])(new double[]{(double)(-1.0), (double)(1.0), (double)(2.0)})), (double)(2.0))));
        System.out.println(_p(swish(((double[])(new double[]{(double)(-2.0)})), (double)(1.0))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
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
        if (v instanceof java.util.Map<?, ?>) {
            StringBuilder sb = new StringBuilder("{");
            boolean first = true;
            for (java.util.Map.Entry<?, ?> e : ((java.util.Map<?, ?>) v).entrySet()) {
                if (!first) sb.append(", ");
                sb.append(_p(e.getKey()));
                sb.append("=");
                sb.append(_p(e.getValue()));
                first = false;
            }
            sb.append("}");
            return sb.toString();
        }
        if (v instanceof java.util.List<?>) {
            StringBuilder sb = new StringBuilder("[");
            boolean first = true;
            for (Object e : (java.util.List<?>) v) {
                if (!first) sb.append(", ");
                sb.append(_p(e));
                first = false;
            }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
