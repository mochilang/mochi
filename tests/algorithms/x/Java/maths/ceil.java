public class Main {
    static double[] values;

    static long ceil(double x) {
        long truncated = (long)(((Number)(x)).intValue());
        double frac_1 = (double)((double)(x) - (double)((((Number)(truncated)).doubleValue())));
        if ((double)(frac_1) <= (double)(0.0)) {
            return truncated;
        }
        return (long)(truncated) + 1L;
    }
    public static void main(String[] args) {
        values = ((double[])(new double[]{1.0, -1.0, 0.0, -0.0, 1.1, -1.1, 1.0, -1.0, 1000000000.0}));
        for (double v : values) {
            System.out.println(ceil((double)(v)));
        }
    }
}
