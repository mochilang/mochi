public class Main {

    static double exp(double x) {
        double term = (double)(1.0);
        double sum_1 = (double)(1.0);
        long n_1 = 1L;
        while ((long)(n_1) < 20L) {
            term = (double)((double)((double)(term) * (double)(x)) / (double)(((Number)(n_1)).doubleValue()));
            sum_1 = (double)((double)(sum_1) + (double)(term));
            n_1 = (long)((long)(n_1) + 1L);
        }
        return sum_1;
    }

    static double[] soboleva_modified_hyperbolic_tangent(double[] vector, double a_value, double b_value, double c_value, double d_value) {
        double[] result = ((double[])(new double[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(vector.length)) {
            double x_1 = (double)(vector[(int)((long)(i_1))]);
            double numerator_1 = (double)((double)(exp((double)((double)(a_value) * (double)(x_1)))) - (double)(exp((double)((double)(-b_value) * (double)(x_1)))));
            double denominator_1 = (double)((double)(exp((double)((double)(c_value) * (double)(x_1)))) + (double)(exp((double)((double)(-d_value) * (double)(x_1)))));
            result = ((double[])(appendDouble(result, (double)((double)(numerator_1) / (double)(denominator_1)))));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return result;
    }

    static void main() {
        double[] vector = ((double[])(new double[]{5.4, -2.4, 6.3, -5.23, 3.27, 0.56}));
        double[] res_1 = ((double[])(soboleva_modified_hyperbolic_tangent(((double[])(vector)), (double)(0.2), (double)(0.4), (double)(0.6), (double)(0.8))));
        json(res_1);
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

    static void json(Object v) {
        System.out.println(_json(v));
    }

    static String _json(Object v) {
        if (v == null) return "null";
        if (v instanceof String) {
            String s = (String)v;
            s = s.replace("\\", "\\\\").replace("\"", "\\\"");
            return "\"" + s + "\"";
        }
        if (v instanceof Number || v instanceof Boolean) {
            return String.valueOf(v);
        }
        if (v instanceof int[]) {
            int[] a = (int[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(a[i]); }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof double[]) {
            double[] a = (double[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(a[i]); }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof boolean[]) {
            boolean[] a = (boolean[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(a[i]); }
            sb.append("]");
            return sb.toString();
        }
        if (v.getClass().isArray()) {
            Object[] a = (Object[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(_json(a[i])); }
            sb.append("]");
            return sb.toString();
        }
        String s = String.valueOf(v);
        s = s.replace("\\", "\\\\").replace("\"", "\\\"");
        return "\"" + s + "\"";
    }
}
