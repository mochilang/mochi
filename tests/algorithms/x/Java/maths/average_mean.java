public class Main {

    static double mean(double[] nums) {
        if (nums.length == 0) {
            throw new RuntimeException(String.valueOf("List is empty"));
        }
        double total_1 = 0.0;
        long i_1 = 0;
        while (i_1 < nums.length) {
            total_1 = total_1 + nums[(int)(i_1)];
            i_1 = i_1 + 1;
        }
        return total_1 / (((Number)(nums.length)).doubleValue());
    }
    public static void main(String[] args) {
        System.out.println(_p(mean(((double[])(new double[]{3.0, 6.0, 9.0, 12.0, 15.0, 18.0, 21.0})))));
        System.out.println(_p(mean(((double[])(new double[]{5.0, 10.0, 15.0, 20.0, 25.0, 30.0, 35.0})))));
        System.out.println(_p(mean(((double[])(new double[]{1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0})))));
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
