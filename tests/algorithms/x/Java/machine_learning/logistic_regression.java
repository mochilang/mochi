public class Main {
    static double[][] x;
    static double[] y_1;
    static double alpha;
    static int iterations;
    static double[] theta_1;

    static double expApprox(double x) {
        double y = x;
        boolean is_neg = false;
        if (x < 0.0) {
            is_neg = true;
            y = -x;
        }
        double term = 1.0;
        double sum = 1.0;
        int n = 1;
        while (n < 30) {
            term = term * y / (((Number)(n)).doubleValue());
            sum = sum + term;
            n = n + 1;
        }
        if (((Boolean)(is_neg))) {
            return 1.0 / sum;
        }
        return sum;
    }

    static double sigmoid(double z) {
        return 1.0 / (1.0 + expApprox(-z));
    }

    static double dot(double[] a, double[] b) {
        double s = 0.0;
        int i = 0;
        while (i < a.length) {
            s = s + a[i] * b[i];
            i = i + 1;
        }
        return s;
    }

    static double[] zeros(int n) {
        double[] res = ((double[])(new double[]{}));
        int i_1 = 0;
        while (i_1 < n) {
            res = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.stream.DoubleStream.of(0.0)).toArray()));
            i_1 = i_1 + 1;
        }
        return res;
    }

    static double[] logistic_reg(double alpha, double[][] x, double[] y, int iterations) {
        int m = x.length;
        int n_1 = x[0].length;
        double[] theta = ((double[])(zeros(n_1)));
        int iter = 0;
        while (iter < iterations) {
            double[] grad = ((double[])(zeros(n_1)));
            int i_2 = 0;
            while (i_2 < m) {
                double z = dot(((double[])(x[i_2])), ((double[])(theta)));
                double h = sigmoid(z);
                int k = 0;
                while (k < n_1) {
grad[k] = grad[k] + (h - y[i_2]) * x[i_2][k];
                    k = k + 1;
                }
                i_2 = i_2 + 1;
            }
            int k2 = 0;
            while (k2 < n_1) {
theta[k2] = theta[k2] - alpha * grad[k2] / (((Number)(m)).doubleValue());
                k2 = k2 + 1;
            }
            iter = iter + 1;
        }
        return theta;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            x = ((double[][])(new double[][]{new double[]{0.5, 1.5}, new double[]{1.0, 1.0}, new double[]{1.5, 0.5}, new double[]{3.0, 3.5}, new double[]{3.5, 3.0}, new double[]{4.0, 4.0}}));
            y_1 = ((double[])(new double[]{0.0, 0.0, 0.0, 1.0, 1.0, 1.0}));
            alpha = 0.1;
            iterations = 1000;
            theta_1 = ((double[])(logistic_reg(alpha, ((double[][])(x)), ((double[])(y_1)), iterations)));
            for (int i = 0; i < theta_1.length; i++) {
                System.out.println(theta_1[i]);
            }
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
}
