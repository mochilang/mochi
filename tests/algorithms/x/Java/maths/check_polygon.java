public class Main {
    static double[] nums = new double[0];
    static boolean _v;

    static boolean check_polygon(double[] nums) {
        if (nums.length < 2) {
            throw new RuntimeException(String.valueOf("Monogons and Digons are not polygons in the Euclidean space"));
        }
        long i_1 = 0;
        while (i_1 < nums.length) {
            if (nums[(int)(i_1)] <= 0.0) {
                throw new RuntimeException(String.valueOf("All values must be greater than 0"));
            }
            i_1 = i_1 + 1;
        }
        double total_1 = 0.0;
        double max_side_1 = 0.0;
        i_1 = 0;
        while (i_1 < nums.length) {
            double v_1 = nums[(int)(i_1)];
            total_1 = total_1 + v_1;
            if (v_1 > max_side_1) {
                max_side_1 = v_1;
            }
            i_1 = i_1 + 1;
        }
        return max_side_1 < (total_1 - max_side_1);
    }
    public static void main(String[] args) {
        System.out.println(_p(check_polygon(((double[])(new double[]{6.0, 10.0, 5.0})))));
        System.out.println(_p(check_polygon(((double[])(new double[]{3.0, 7.0, 13.0, 2.0})))));
        System.out.println(_p(check_polygon(((double[])(new double[]{1.0, 4.3, 5.2, 12.2})))));
        nums = ((double[])(new double[]{3.0, 7.0, 13.0, 2.0}));
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
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
