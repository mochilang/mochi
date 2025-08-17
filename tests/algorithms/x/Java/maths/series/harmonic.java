public class Main {

    static boolean is_harmonic_series(double[] series) {
        if ((long)(series.length) == 0L) {
            throw new RuntimeException(String.valueOf("Input list must be a non empty list"));
        }
        if ((long)(series.length) == 1L) {
            if ((double)(series[(int)(0L)]) == (double)(0.0)) {
                throw new RuntimeException(String.valueOf("Input series cannot have 0 as an element"));
            }
            return true;
        }
        double[] rec_series_1 = ((double[])(new double[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(series.length)) {
            double val_1 = (double)(series[(int)((long)(i_1))]);
            if ((double)(val_1) == (double)(0.0)) {
                throw new RuntimeException(String.valueOf("Input series cannot have 0 as an element"));
            }
            rec_series_1 = ((double[])(appendDouble(rec_series_1, (double)((double)(1.0) / (double)(val_1)))));
            i_1 = (long)((long)(i_1) + 1L);
        }
        double common_diff_1 = (double)((double)(rec_series_1[(int)(1L)]) - (double)(rec_series_1[(int)(0L)]));
        long idx_1 = 2L;
        while ((long)(idx_1) < (long)(rec_series_1.length)) {
            if ((double)((double)(rec_series_1[(int)((long)(idx_1))]) - (double)(rec_series_1[(int)((long)((long)(idx_1) - 1L))])) != (double)(common_diff_1)) {
                return false;
            }
            idx_1 = (long)((long)(idx_1) + 1L);
        }
        return true;
    }

    static double harmonic_mean(double[] series) {
        if ((long)(series.length) == 0L) {
            throw new RuntimeException(String.valueOf("Input list must be a non empty list"));
        }
        double total_1 = (double)(0.0);
        long i_3 = 0L;
        while ((long)(i_3) < (long)(series.length)) {
            total_1 = (double)((double)(total_1) + (double)((double)(1.0) / (double)(series[(int)((long)(i_3))])));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return (double)((((Number)(series.length)).doubleValue())) / (double)(total_1);
    }
    public static void main(String[] args) {
        System.out.println(is_harmonic_series(((double[])(new double[]{1.0, (double)(2.0) / (double)(3.0), (double)(1.0) / (double)(2.0), (double)(2.0) / (double)(5.0), (double)(1.0) / (double)(3.0)}))));
        System.out.println(is_harmonic_series(((double[])(new double[]{1.0, (double)(2.0) / (double)(3.0), (double)(2.0) / (double)(5.0), (double)(1.0) / (double)(3.0)}))));
        System.out.println(harmonic_mean(((double[])(new double[]{1.0, 4.0, 4.0}))));
        System.out.println(harmonic_mean(((double[])(new double[]{3.0, 6.0, 9.0, 12.0}))));
    }

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
