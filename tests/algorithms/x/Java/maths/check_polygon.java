public class Main {
    static double[] nums = ((double[])(new double[]{3.0, 7.0, 13.0, 2.0}));
    static boolean _v;

    static boolean check_polygon(double[] nums) {
        if ((long)(nums.length) < 2L) {
            throw new RuntimeException(String.valueOf("Monogons and Digons are not polygons in the Euclidean space"));
        }
        long i_1 = 0L;
        while ((long)(i_1) < (long)(nums.length)) {
            if ((double)(nums[(int)((long)(i_1))]) <= (double)(0.0)) {
                throw new RuntimeException(String.valueOf("All values must be greater than 0"));
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        double total_1 = (double)(0.0);
        double max_side_1 = (double)(0.0);
        i_1 = 0L;
        while ((long)(i_1) < (long)(nums.length)) {
            double v_1 = (double)(nums[(int)((long)(i_1))]);
            total_1 = (double)((double)(total_1) + (double)(v_1));
            if ((double)(v_1) > (double)(max_side_1)) {
                max_side_1 = (double)(v_1);
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return (double)(max_side_1) < (double)(((double)(total_1) - (double)(max_side_1)));
    }
    public static void main(String[] args) {
        System.out.println(_p(check_polygon(((double[])(new double[]{6.0, 10.0, 5.0})))));
        System.out.println(_p(check_polygon(((double[])(new double[]{3.0, 7.0, 13.0, 2.0})))));
        System.out.println(_p(check_polygon(((double[])(new double[]{1.0, 4.3, 5.2, 12.2})))));
        _v = check_polygon(((double[])(nums)));
        System.out.println(_p(nums));
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
