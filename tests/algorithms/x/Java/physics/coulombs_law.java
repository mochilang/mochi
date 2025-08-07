public class Main {
    static double K;

    static String format2(double x) {
        String sign = String.valueOf(x < 0.0 ? "-" : "");
        double y = x < 0.0 ? -x : x;
        double m = 100.0;
        double scaled = y * m;
        int i = ((Number)(scaled)).intValue();
        if (scaled - (((Number)(i)).doubleValue()) >= 0.5) {
            i = i + 1;
        }
        int int_part = Math.floorDiv(i, 100);
        int frac_part = Math.floorMod(i, 100);
        String frac_str = _p(frac_part);
        if (frac_part < 10) {
            frac_str = "0" + frac_str;
        }
        return sign + _p(int_part) + "." + frac_str;
    }

    static double coulombs_law(double q1, double q2, double radius) {
        if (radius <= 0.0) {
            throw new RuntimeException(String.valueOf("radius must be positive"));
        }
        double force = K * q1 * q2 / (radius * radius);
        return force;
    }
    public static void main(String[] args) {
        K = 8.9875517923e+09;
        System.out.println(format2(coulombs_law(15.5, 20.0, 15.0)));
        System.out.println(format2(coulombs_law(1.0, 15.0, 5.0)));
        System.out.println(format2(coulombs_law(20.0, -50.0, 15.0)));
        System.out.println(format2(coulombs_law(-5.0, -8.0, 10.0)));
        System.out.println(format2(coulombs_law(50.0, 100.0, 50.0)));
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
        return String.valueOf(v);
    }
}
