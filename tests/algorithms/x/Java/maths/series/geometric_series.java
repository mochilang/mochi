public class Main {

    static double[] geometric_series(double nth_term, double start_term_a, double common_ratio_r) {
        long n = (long)(((Number)(nth_term)).intValue());
        if ((long)(n) <= 0L || (double)(start_term_a) == (double)(0.0) || (double)(common_ratio_r) == (double)(0.0)) {
            return new double[]{};
        }
        double[] series_1 = ((double[])(new double[]{}));
        double current_1 = (double)(start_term_a);
        long i_1 = 0L;
        while ((long)(i_1) < (long)(n)) {
            series_1 = ((double[])(appendDouble(series_1, (double)(current_1))));
            current_1 = (double)((double)(current_1) * (double)(common_ratio_r));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return series_1;
    }
    public static void main(String[] args) {
        System.out.println(geometric_series((double)(4.0), (double)(2.0), (double)(2.0)));
        System.out.println(geometric_series((double)(4.0), (double)(2.0), (double)(-2.0)));
        System.out.println(geometric_series((double)(4.0), (double)(-2.0), (double)(2.0)));
        System.out.println(geometric_series((double)(-4.0), (double)(2.0), (double)(2.0)));
        System.out.println(geometric_series((double)(0.0), (double)(100.0), (double)(500.0)));
        System.out.println(geometric_series((double)(1.0), (double)(1.0), (double)(1.0)));
    }

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
