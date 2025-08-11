public class Main {

    static double floor(double x) {
        long i = ((Number)(x)).intValue();
        if ((((Number)(i)).doubleValue()) > x) {
            i = i - 1;
        }
        return ((Number)(i)).doubleValue();
    }

    static double pow10(long n) {
        double result = 1.0;
        long i_2 = 0;
        while (i_2 < n) {
            result = result * 10.0;
            i_2 = i_2 + 1;
        }
        return result;
    }

    static double round(double x, long n) {
        double m = pow10(n);
        double y_1 = ((Number)(floor(x * m + 0.5))).doubleValue();
        return y_1 / m;
    }

    static double sqrtApprox(double x) {
        double guess = x;
        long i_4 = 0;
        while (i_4 < 20) {
            guess = (guess + x / guess) / 2.0;
            i_4 = i_4 + 1;
        }
        return guess;
    }

    static double mean(double[] data) {
        double total = 0.0;
        long i_6 = 0;
        long n_1 = data.length;
        while (i_6 < n_1) {
            total = total + data[(int)(i_6)];
            i_6 = i_6 + 1;
        }
        return total / (((Number)(n_1)).doubleValue());
    }

    static double stdev(double[] data) {
        long n_2 = data.length;
        if (n_2 <= 1) {
            throw new RuntimeException(String.valueOf("data length must be > 1"));
        }
        double m_2 = mean(((double[])(data)));
        double sum_sq_1 = 0.0;
        long i_8 = 0;
        while (i_8 < n_2) {
            double diff_1 = data[(int)(i_8)] - m_2;
            sum_sq_1 = sum_sq_1 + diff_1 * diff_1;
            i_8 = i_8 + 1;
        }
        return sqrtApprox(sum_sq_1 / (((Number)((n_2 - 1))).doubleValue()));
    }

    static double[] normalization(double[] data, long ndigits) {
        double x_min = ((Number)(_min(data))).doubleValue();
        double x_max_1 = ((Number)(_max(data))).doubleValue();
        double denom_1 = x_max_1 - x_min;
        double[] result_2 = ((double[])(new double[]{}));
        long i_10 = 0;
        long n_4 = data.length;
        while (i_10 < n_4) {
            double norm_1 = (data[(int)(i_10)] - x_min) / denom_1;
            result_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(result_2), java.util.stream.DoubleStream.of(round(norm_1, ndigits))).toArray()));
            i_10 = i_10 + 1;
        }
        return result_2;
    }

    static double[] standardization(double[] data, long ndigits) {
        double mu = mean(((double[])(data)));
        double sigma_1 = stdev(((double[])(data)));
        double[] result_4 = ((double[])(new double[]{}));
        long i_12 = 0;
        long n_6 = data.length;
        while (i_12 < n_6) {
            double z_1 = (data[(int)(i_12)] - mu) / sigma_1;
            result_4 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(result_4), java.util.stream.DoubleStream.of(round(z_1, ndigits))).toArray()));
            i_12 = i_12 + 1;
        }
        return result_4;
    }
    public static void main(String[] args) {
        System.out.println(_p(normalization(((double[])(new double[]{2.0, 7.0, 10.0, 20.0, 30.0, 50.0})), 3)));
        System.out.println(_p(normalization(((double[])(new double[]{5.0, 10.0, 15.0, 20.0, 25.0})), 3)));
        System.out.println(_p(standardization(((double[])(new double[]{2.0, 7.0, 10.0, 20.0, 30.0, 50.0})), 3)));
        System.out.println(_p(standardization(((double[])(new double[]{5.0, 10.0, 15.0, 20.0, 25.0})), 3)));
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
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
