public class Main {
    static double[] values;

    static long ceil(double x) {
        long truncated = ((Number)(x)).intValue();
        double frac_1 = x - (((Number)(truncated)).doubleValue());
        if (frac_1 <= 0.0) {
            return truncated;
        }
        return truncated + 1;
    }
    public static void main(String[] args) {
        values = ((double[])(new double[]{1.0, -1.0, 0.0, -0.0, 1.1, -1.1, 1.0, -1.0, 1000000000.0}));
        for (double v : values) {
            System.out.println(ceil(v));
        }
    }
}
