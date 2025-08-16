public class Main {
    static double[][] x = ((double[][])(new double[][]{new double[]{0.5, 1.5}, new double[]{1.0, 1.0}, new double[]{1.5, 0.5}, new double[]{3.0, 3.5}, new double[]{3.5, 3.0}, new double[]{4.0, 4.0}}));
    static double[] y_1 = ((double[])(new double[]{0.0, 0.0, 0.0, 1.0, 1.0, 1.0}));
    static double alpha = (double)(0.1);
    static long iterations = 1000L;
    static double[] theta_2;

    static double expApprox(double x) {
        double y = (double)(x);
        boolean is_neg_1 = false;
        if ((double)(x) < (double)(0.0)) {
            is_neg_1 = true;
            y = (double)(-x);
        }
        double term_1 = (double)(1.0);
        double sum_1 = (double)(1.0);
        long n_1 = 1L;
        while ((long)(n_1) < 30L) {
            term_1 = (double)((double)((double)(term_1) * (double)(y)) / (double)((((Number)(n_1)).doubleValue())));
            sum_1 = (double)((double)(sum_1) + (double)(term_1));
            n_1 = (long)((long)(n_1) + 1L);
        }
        if (is_neg_1) {
            return (double)(1.0) / (double)(sum_1);
        }
        return sum_1;
    }

    static double sigmoid(double z) {
        return (double)(1.0) / (double)(((double)(1.0) + (double)(expApprox((double)(-z)))));
    }

    static double dot(double[] a, double[] b) {
        double s = (double)(0.0);
        long i_1 = 0L;
        while ((long)(i_1) < (long)(a.length)) {
            s = (double)((double)(s) + (double)((double)(a[(int)((long)(i_1))]) * (double)(b[(int)((long)(i_1))])));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return s;
    }

    static double[] zeros(long n) {
        double[] res = ((double[])(new double[]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(n)) {
            res = ((double[])(appendDouble(res, (double)(0.0))));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return res;
    }

    static double[] logistic_reg(double alpha, double[][] x, double[] y, long iterations) {
        long m = (long)(x.length);
        long n_3 = (long)(x[(int)(0L)].length);
        double[] theta_1 = ((double[])(zeros((long)(n_3))));
        long iter_1 = 0L;
        while ((long)(iter_1) < (long)(iterations)) {
            double[] grad_1 = ((double[])(zeros((long)(n_3))));
            long i_5 = 0L;
            while ((long)(i_5) < (long)(m)) {
                double z_1 = (double)(dot(((double[])(x[(int)((long)(i_5))])), ((double[])(theta_1))));
                double h_1 = (double)(sigmoid((double)(z_1)));
                long k_1 = 0L;
                while ((long)(k_1) < (long)(n_3)) {
grad_1[(int)((long)(k_1))] = (double)((double)(grad_1[(int)((long)(k_1))]) + (double)((double)(((double)(h_1) - (double)(y[(int)((long)(i_5))]))) * (double)(x[(int)((long)(i_5))][(int)((long)(k_1))])));
                    k_1 = (long)((long)(k_1) + 1L);
                }
                i_5 = (long)((long)(i_5) + 1L);
            }
            long k2_1 = 0L;
            while ((long)(k2_1) < (long)(n_3)) {
theta_1[(int)((long)(k2_1))] = (double)((double)(theta_1[(int)((long)(k2_1))]) - (double)((double)((double)(alpha) * (double)(grad_1[(int)((long)(k2_1))])) / (double)((((Number)(m)).doubleValue()))));
                k2_1 = (long)((long)(k2_1) + 1L);
            }
            iter_1 = (long)((long)(iter_1) + 1L);
        }
        return theta_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            theta_2 = ((double[])(logistic_reg((double)(alpha), ((double[][])(x)), ((double[])(y_1)), (long)(iterations))));
            for (int i = 0; i < theta_2.length; i++) {
                System.out.println(theta_2[(int)((long)(i))]);
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

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
