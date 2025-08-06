public class Main {

    static double floor(double x) {
        int i = ((Number)(x)).intValue();
        if ((((Number)(i)).doubleValue()) > x) {
            i = i - 1;
        }
        return ((Number)(i)).doubleValue();
    }

    static double pow10(int n) {
        double result = 1.0;
        int i_1 = 0;
        while (i_1 < n) {
            result = result * 10.0;
            i_1 = i_1 + 1;
        }
        return result;
    }

    static double round(double x, int n) {
        double m = pow10(n);
        double y = ((Number)(floor(x * m + 0.5))).doubleValue();
        return y / m;
    }

    static double sqrtApprox(double x) {
        double guess = x;
        int i_2 = 0;
        while (i_2 < 20) {
            guess = (guess + x / guess) / 2.0;
            i_2 = i_2 + 1;
        }
        return guess;
    }

    static double mean(double[] data) {
        double total = 0.0;
        int i_3 = 0;
        int n = data.length;
        while (i_3 < n) {
            total = total + data[i_3];
            i_3 = i_3 + 1;
        }
        return total / (((Number)(n)).doubleValue());
    }

    static double stdev(double[] data) {
        int n_1 = data.length;
        if (n_1 <= 1) {
            throw new RuntimeException(String.valueOf("data length must be > 1"));
        }
        double m_1 = mean(((double[])(data)));
        double sum_sq = 0.0;
        int i_4 = 0;
        while (i_4 < n_1) {
            double diff = data[i_4] - m_1;
            sum_sq = sum_sq + diff * diff;
            i_4 = i_4 + 1;
        }
        return sqrtApprox(sum_sq / (((Number)((n_1 - 1))).doubleValue()));
    }

    static double[] normalization(double[] data, int ndigits) {
        double x_min = ((Number)(_min(data))).doubleValue();
        double x_max = ((Number)(_max(data))).doubleValue();
        double denom = x_max - x_min;
        double[] result_1 = ((double[])(new double[]{}));
        int i_5 = 0;
        int n_2 = data.length;
        while (i_5 < n_2) {
            double norm = (data[i_5] - x_min) / denom;
            result_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(result_1), java.util.stream.DoubleStream.of(round(norm, ndigits))).toArray()));
            i_5 = i_5 + 1;
        }
        return result_1;
    }

    static double[] standardization(double[] data, int ndigits) {
        double mu = mean(((double[])(data)));
        double sigma = stdev(((double[])(data)));
        double[] result_2 = ((double[])(new double[]{}));
        int i_6 = 0;
        int n_3 = data.length;
        while (i_6 < n_3) {
            double z = (data[i_6] - mu) / sigma;
            result_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(result_2), java.util.stream.DoubleStream.of(round(z, ndigits))).toArray()));
            i_6 = i_6 + 1;
        }
        return result_2;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(normalization(((double[])(new double[]{2.0, 7.0, 10.0, 20.0, 30.0, 50.0})), 3)));
            System.out.println(_p(normalization(((double[])(new double[]{5.0, 10.0, 15.0, 20.0, 25.0})), 3)));
            System.out.println(_p(standardization(((double[])(new double[]{2.0, 7.0, 10.0, 20.0, 30.0, 50.0})), 3)));
            System.out.println(_p(standardization(((double[])(new double[]{5.0, 10.0, 15.0, 20.0, 25.0})), 3)));
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

    static int _min(int[] a) {
        int m = a[0];
        for (int i = 1; i < a.length; i++) if (a[i] < m) m = a[i];
        return m;
    }

    static int _max(int[] a) {
        int m = a[0];
        for (int i = 1; i < a.length; i++) if (a[i] > m) m = a[i];
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
        return String.valueOf(v);
    }
}
