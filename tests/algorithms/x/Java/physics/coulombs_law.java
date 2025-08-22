public class Main {
    static double K = (double)(8.9875517923e+09);

    static String format2(double x) {
        String sign = String.valueOf((double)(x) < (double)(0.0) ? "-" : "");
        double y_1 = (double)((double)(x) < (double)(0.0) ? -x : x);
        double m_1 = (double)(100.0);
        double scaled_1 = (double)((double)(y_1) * (double)(m_1));
        long i_1 = (long)(((Number)(scaled_1)).intValue());
        if ((double)((double)(scaled_1) - (double)((((Number)(i_1)).doubleValue()))) >= (double)(0.5)) {
            i_1 = (long)((long)(i_1) + 1L);
        }
        long int_part_1 = Math.floorDiv(i_1, 100);
        long frac_part_1 = Math.floorMod(i_1, 100);
        String frac_str_1 = _p(frac_part_1);
        if ((long)(frac_part_1) < 10L) {
            frac_str_1 = "0" + frac_str_1;
        }
        return sign + _p(int_part_1) + "." + frac_str_1;
    }

    static double coulombs_law(double q1, double q2, double radius) {
        if ((double)(radius) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("radius must be positive"));
        }
        double force_1 = (double)((double)((double)((double)(K) * (double)(q1)) * (double)(q2)) / (double)(((double)(radius) * (double)(radius))));
        return force_1;
    }
    public static void main(String[] args) {
        System.out.println(format2((double)(coulombs_law((double)(15.5), (double)(20.0), (double)(15.0)))));
        System.out.println(format2((double)(coulombs_law((double)(1.0), (double)(15.0), (double)(5.0)))));
        System.out.println(format2((double)(coulombs_law((double)(20.0), (double)(-50.0), (double)(15.0)))));
        System.out.println(format2((double)(coulombs_law((double)(-5.0), (double)(-8.0), (double)(10.0)))));
        System.out.println(format2((double)(coulombs_law((double)(50.0), (double)(100.0), (double)(50.0)))));
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
