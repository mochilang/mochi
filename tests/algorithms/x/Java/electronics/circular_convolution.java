public class Main {
    static double[] example1;
    static double[] example2;
    static double[] example3;
    static double[] example4;

    static double floor(double x) {
        java.math.BigInteger i = new java.math.BigInteger(String.valueOf(((Number)(x)).intValue()));
        if ((double)((((Number)(i)).doubleValue())) > (double)(x)) {
            i = new java.math.BigInteger(String.valueOf(i.subtract(java.math.BigInteger.valueOf(1))));
        }
        return (double)(((Number)(i)).doubleValue());
    }

    static double pow10(java.math.BigInteger n) {
        double p = (double)(1.0);
        java.math.BigInteger i_2 = java.math.BigInteger.valueOf(0);
        while (i_2.compareTo(n) < 0) {
            p = (double)((double)(p) * (double)(10.0));
            i_2 = new java.math.BigInteger(String.valueOf(i_2.add(java.math.BigInteger.valueOf(1))));
        }
        return (double)(p);
    }

    static double roundn(double x, java.math.BigInteger n) {
        double m = (double)(pow10(new java.math.BigInteger(String.valueOf(n))));
        return (double)(Math.floor((double)((double)(x) * (double)(m)) + (double)(0.5)) / (double)(m));
    }

    static double[] pad(double[] signal, java.math.BigInteger target) {
        double[] s = ((double[])(signal));
        while (new java.math.BigInteger(String.valueOf(s.length)).compareTo(target) < 0) {
            s = ((double[])(appendDouble(s, (double)(0.0))));
        }
        return ((double[])(s));
    }

    static double[] circular_convolution(double[] a, double[] b) {
        java.math.BigInteger n1 = new java.math.BigInteger(String.valueOf(a.length));
        java.math.BigInteger n2_1 = new java.math.BigInteger(String.valueOf(b.length));
        java.math.BigInteger n_1 = new java.math.BigInteger(String.valueOf(n1.compareTo(n2_1) > 0 ? n1 : n2_1));
        double[] x_1 = ((double[])(pad(((double[])(a)), new java.math.BigInteger(String.valueOf(n_1)))));
        double[] y_1 = ((double[])(pad(((double[])(b)), new java.math.BigInteger(String.valueOf(n_1)))));
        double[] res_1 = ((double[])(new double[]{}));
        java.math.BigInteger i_4 = java.math.BigInteger.valueOf(0);
        while (i_4.compareTo(n_1) < 0) {
            double sum_1 = (double)(0.0);
            java.math.BigInteger k_1 = java.math.BigInteger.valueOf(0);
            while (k_1.compareTo(n_1) < 0) {
                java.math.BigInteger j_1 = new java.math.BigInteger(String.valueOf((i_4.subtract(k_1)).remainder(n_1)));
                java.math.BigInteger idx_1 = new java.math.BigInteger(String.valueOf(j_1.compareTo(java.math.BigInteger.valueOf(0)) < 0 ? j_1.add(n_1) : j_1));
                sum_1 = (double)((double)(sum_1) + (double)((double)(x_1[_idx((x_1).length, ((java.math.BigInteger)(k_1)).longValue())]) * (double)(y_1[_idx((y_1).length, ((java.math.BigInteger)(idx_1)).longValue())])));
                k_1 = new java.math.BigInteger(String.valueOf(k_1.add(java.math.BigInteger.valueOf(1))));
            }
            res_1 = ((double[])(appendDouble(res_1, (double)(roundn((double)(sum_1), java.math.BigInteger.valueOf(2))))));
            i_4 = new java.math.BigInteger(String.valueOf(i_4.add(java.math.BigInteger.valueOf(1))));
        }
        return ((double[])(res_1));
    }
    public static void main(String[] args) {
        example1 = ((double[])(circular_convolution(((double[])(new double[]{(double)(2.0), (double)(1.0), (double)(2.0), (double)(-1.0)})), ((double[])(new double[]{(double)(1.0), (double)(2.0), (double)(3.0), (double)(4.0)})))));
        System.out.println(_p(example1));
        example2 = ((double[])(circular_convolution(((double[])(new double[]{(double)(0.2), (double)(0.4), (double)(0.6), (double)(0.8), (double)(1.0), (double)(1.2), (double)(1.4), (double)(1.6)})), ((double[])(new double[]{(double)(0.1), (double)(0.3), (double)(0.5), (double)(0.7), (double)(0.9), (double)(1.1), (double)(1.3), (double)(1.5)})))));
        System.out.println(_p(example2));
        example3 = ((double[])(circular_convolution(((double[])(new double[]{(double)(-1.0), (double)(1.0), (double)(2.0), (double)(-2.0)})), ((double[])(new double[]{(double)(0.5), (double)(1.0), (double)(-1.0), (double)(2.0), (double)(0.75)})))));
        System.out.println(_p(example3));
        example4 = ((double[])(circular_convolution(((double[])(new double[]{(double)(1.0), (double)(-1.0), (double)(2.0), (double)(3.0), (double)(-1.0)})), ((double[])(new double[]{(double)(1.0), (double)(2.0), (double)(3.0)})))));
        System.out.println(_p(example4));
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
