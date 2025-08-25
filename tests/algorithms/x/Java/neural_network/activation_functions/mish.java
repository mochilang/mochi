public class Main {

    static double exp_approx(double x) {
        boolean neg = false;
        double y_1 = (double)(x);
        if ((double)(x) < (double)(0.0)) {
            neg = true;
            y_1 = (double)(-x);
        }
        double term_1 = (double)(1.0);
        double sum_1 = (double)(1.0);
        java.math.BigInteger n_1 = java.math.BigInteger.valueOf(1);
        while (n_1.compareTo(java.math.BigInteger.valueOf(30)) < 0) {
            term_1 = (double)((double)((double)(term_1) * (double)(y_1)) / (double)((((Number)(n_1)).doubleValue())));
            sum_1 = (double)((double)(sum_1) + (double)(term_1));
            n_1 = new java.math.BigInteger(String.valueOf(n_1.add(java.math.BigInteger.valueOf(1))));
        }
        if (neg) {
            return (double)((double)(1.0) / (double)(sum_1));
        }
        return (double)(sum_1);
    }

    static double ln_series(double x) {
        double t = (double)((double)(((double)(x) - (double)(1.0))) / (double)(((double)(x) + (double)(1.0))));
        double term_3 = (double)(t);
        double acc_1 = (double)(0.0);
        java.math.BigInteger n_3 = java.math.BigInteger.valueOf(1);
        while (n_3.compareTo(java.math.BigInteger.valueOf(19)) <= 0) {
            acc_1 = (double)((double)(acc_1) + (double)((double)(term_3) / (double)((((Number)(n_3)).doubleValue()))));
            term_3 = (double)((double)((double)(term_3) * (double)(t)) * (double)(t));
            n_3 = new java.math.BigInteger(String.valueOf(n_3.add(java.math.BigInteger.valueOf(2))));
        }
        return (double)((double)(2.0) * (double)(acc_1));
    }

    static double ln(double x) {
        double y_2 = (double)(x);
        java.math.BigInteger k_1 = java.math.BigInteger.valueOf(0);
        while ((double)(y_2) >= (double)(10.0)) {
            y_2 = (double)((double)(y_2) / (double)(10.0));
            k_1 = new java.math.BigInteger(String.valueOf(k_1.add(java.math.BigInteger.valueOf(1))));
        }
        while ((double)(y_2) < (double)(1.0)) {
            y_2 = (double)((double)(y_2) * (double)(10.0));
            k_1 = new java.math.BigInteger(String.valueOf(k_1.subtract(java.math.BigInteger.valueOf(1))));
        }
        return (double)((double)(ln_series((double)(y_2))) + (double)((double)((((Number)(k_1)).doubleValue())) * (double)(ln_series((double)(10.0)))));
    }

    static double softplus(double x) {
        return (double)(Math.log((double)(1.0) + (double)(exp_approx((double)(x)))));
    }

    static double tanh_approx(double x) {
        return (double)((double)(((double)(2.0) / (double)(((double)(1.0) + (double)(exp_approx((double)((double)(-2.0) * (double)(x)))))))) - (double)(1.0));
    }

    static double[] mish(double[] vector) {
        double[] result = ((double[])(new double[]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(vector.length))) < 0) {
            double x_1 = (double)(vector[_idx((vector).length, ((java.math.BigInteger)(i_1)).longValue())]);
            double sp_1 = (double)(softplus((double)(x_1)));
            double y_4 = (double)((double)(x_1) * (double)(tanh_approx((double)(sp_1))));
            result = ((double[])(appendDouble(result, (double)(y_4))));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[])(result));
    }

    static void main() {
        double[] v1 = ((double[])(new double[]{(double)(2.3), (double)(0.6), (double)(-2.0), (double)(-3.8)}));
        double[] v2_1 = ((double[])(new double[]{(double)(-9.2), (double)(-0.3), (double)(0.45), (double)(-4.56)}));
        System.out.println(_p(mish(((double[])(v1)))));
        System.out.println(_p(mish(((double[])(v2_1)))));
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
