public class Main {

    static boolean is_geometric_series(double[] series) {
        if ((long)(series.length) == 0L) {
            throw new RuntimeException(String.valueOf("Input list must be a non empty list"));
        }
        if ((long)(series.length) == 1L) {
            return true;
        }
        if ((double)(series[(int)(0L)]) == (double)(0.0)) {
            return false;
        }
        double ratio_1 = (double)((double)(series[(int)(1L)]) / (double)(series[(int)(0L)]));
        long i_1 = 0L;
        while ((long)(i_1) < (long)((long)(series.length) - 1L)) {
            if ((double)(series[(int)((long)(i_1))]) == (double)(0.0)) {
                return false;
            }
            if ((double)((double)(series[(int)((long)((long)(i_1) + 1L))]) / (double)(series[(int)((long)(i_1))])) != (double)(ratio_1)) {
                return false;
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return true;
    }

    static double geometric_mean(double[] series) {
        if ((long)(series.length) == 0L) {
            throw new RuntimeException(String.valueOf("Input list must be a non empty list"));
        }
        double product_1 = (double)(1.0);
        long i_3 = 0L;
        while ((long)(i_3) < (long)(series.length)) {
            product_1 = (double)((double)(product_1) * (double)(series[(int)((long)(i_3))]));
            i_3 = (long)((long)(i_3) + 1L);
        }
        long n_1 = (long)(series.length);
        return nth_root((double)(product_1), (long)(n_1));
    }

    static double pow_float(double base, long exp) {
        double result = (double)(1.0);
        long i_5 = 0L;
        while ((long)(i_5) < (long)(exp)) {
            result = (double)((double)(result) * (double)(base));
            i_5 = (long)((long)(i_5) + 1L);
        }
        return result;
    }

    static double nth_root(double value, long n) {
        if ((double)(value) == (double)(0.0)) {
            return 0.0;
        }
        double low_1 = (double)(0.0);
        double high_1 = (double)(value);
        if ((double)(value) < (double)(1.0)) {
            high_1 = (double)(1.0);
        }
        double mid_1 = (double)((double)(((double)(low_1) + (double)(high_1))) / (double)(2.0));
        long i_7 = 0L;
        while ((long)(i_7) < 40L) {
            double mp_1 = (double)(pow_float((double)(mid_1), (long)(n)));
            if ((double)(mp_1) > (double)(value)) {
                high_1 = (double)(mid_1);
            } else {
                low_1 = (double)(mid_1);
            }
            mid_1 = (double)((double)(((double)(low_1) + (double)(high_1))) / (double)(2.0));
            i_7 = (long)((long)(i_7) + 1L);
        }
        return mid_1;
    }

    static void test_geometric() {
        double[] a = ((double[])(new double[]{2.0, 4.0, 8.0}));
        if (!(Boolean)is_geometric_series(((double[])(a)))) {
            throw new RuntimeException(String.valueOf("expected geometric series"));
        }
        double[] b_1 = ((double[])(new double[]{1.0, 2.0, 3.0}));
        if (is_geometric_series(((double[])(b_1)))) {
            throw new RuntimeException(String.valueOf("expected non geometric series"));
        }
    }

    static void main() {
        test_geometric();
        System.out.println(geometric_mean(((double[])(new double[]{2.0, 4.0, 8.0}))));
    }
    public static void main(String[] args) {
        main();
    }
}
