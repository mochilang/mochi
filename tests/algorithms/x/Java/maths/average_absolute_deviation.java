public class Main {

    static double abs_float(double x) {
        if ((double)(x) < (double)(0.0)) {
            return -x;
        }
        return x;
    }

    static double average_absolute_deviation(long[] nums) {
        if ((long)(nums.length) == 0L) {
            throw new RuntimeException(String.valueOf("List is empty"));
        }
        long sum_1 = 0L;
        for (long x : nums) {
            sum_1 = (long)((long)(sum_1) + (long)(x));
        }
        double n_1 = (double)(((Number)(nums.length)).doubleValue());
        double mean_1 = (double)((double)((((Number)(sum_1)).doubleValue())) / (double)(n_1));
        double dev_sum_1 = (double)(0.0);
        for (long x : nums) {
            dev_sum_1 = (double)((double)(dev_sum_1) + (double)(abs_float((double)((double)((((Number)(x)).doubleValue())) - (double)(mean_1)))));
        }
        return (double)(dev_sum_1) / (double)(n_1);
    }
    public static void main(String[] args) {
        System.out.println(_p(average_absolute_deviation(((long[])(new long[]{0})))));
        System.out.println(_p(average_absolute_deviation(((long[])(new long[]{4, 1, 3, 2})))));
        System.out.println(_p(average_absolute_deviation(((long[])(new long[]{2, 70, 6, 50, 20, 8, 4, 0})))));
        System.out.println(_p(average_absolute_deviation(((long[])(new long[]{-20, 0, 30, 15})))));
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
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
