public class Main {

    static double exp(double x) {
        double term = 1.0;
        double sum_1 = 1.0;
        long n_1 = 1L;
        while ((long)(n_1) < (long)(20)) {
            term = term * (double)(x) / ((Number)(n_1)).doubleValue();
            sum_1 = sum_1 + term;
            n_1 = (long)((long)(n_1) + (long)(1));
        }
        return sum_1;
    }

    static double[] soboleva_modified_hyperbolic_tangent(double[] vector, double a_value, double b_value, double c_value, double d_value) {
        double[] result = ((double[])(new double[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(vector.length)) {
            double x_1 = (double)(vector[(int)((long)(i_1))]);
            double numerator_1 = (double)(exp((double)(a_value) * (double)(x_1))) - (double)(exp((double)(-b_value) * (double)(x_1)));
            double denominator_1 = (double)(exp((double)(c_value) * (double)(x_1))) + (double)(exp((double)(-d_value) * (double)(x_1)));
            result = ((double[])(appendDouble(result, numerator_1 / denominator_1)));
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        return result;
    }

    static void main() {
        double[] vector = ((double[])(new double[]{5.4, -2.4, 6.3, -5.23, 3.27, 0.56}));
        double[] res_1 = ((double[])(soboleva_modified_hyperbolic_tangent(((double[])(vector)), 0.2, 0.4, 0.6, 0.8)));
        json(res_1);
    }
    public static void main(String[] args) {
        main();
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
