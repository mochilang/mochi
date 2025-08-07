public class Main {

    static double pow10(int n) {
        double p = 1.0;
        if (n >= 0) {
            int i = 0;
            while (i < n) {
                p = p * 10.0;
                i = i + 1;
            }
        } else {
            int i_1 = 0;
            while (i_1 > n) {
                p = p / 10.0;
                i_1 = i_1 - 1;
            }
        }
        return p;
    }

    static double floor(double x) {
        int i_2 = ((Number)(x)).intValue();
        double f = ((Number)(i_2)).doubleValue();
        if (f > x) {
            return ((Number)((i_2 - 1))).doubleValue();
        }
        return f;
    }

    static String format_scientific_3(double x) {
        if (x == 0.0) {
            return "0.000e+00";
        }
        String sign = "";
        double num = x;
        if (num < 0.0) {
            sign = "-";
            num = -num;
        }
        int exp = 0;
        while (num >= 10.0) {
            num = num / 10.0;
            exp = exp + 1;
        }
        while (num < 1.0) {
            num = num * 10.0;
            exp = exp - 1;
        }
        double temp = floor(num * 1000.0 + 0.5);
        int scaled = ((Number)(temp)).intValue();
        if (scaled == 10000) {
            scaled = 1000;
            exp = exp + 1;
        }
        int int_part = Math.floorDiv(scaled, 1000);
        int frac_part = Math.floorMod(scaled, 1000);
        String frac_str = _p(frac_part);
        while (_runeLen(frac_str) < 3) {
            frac_str = "0" + frac_str;
        }
        String mantissa = _p(int_part) + "." + frac_str;
        String exp_sign = "+";
        int exp_abs = exp;
        if (exp < 0) {
            exp_sign = "-";
            exp_abs = -exp;
        }
        String exp_str = _p(exp_abs);
        if (exp_abs < 10) {
            exp_str = "0" + exp_str;
        }
        return sign + mantissa + "e" + exp_sign + exp_str;
    }

    static String orbital_transfer_work(double mass_central, double mass_object, double r_initial, double r_final) {
        double G = 6.6743 * pow10(-11);
        if (r_initial <= 0.0 || r_final <= 0.0) {
            throw new RuntimeException(String.valueOf("Orbital radii must be greater than zero."));
        }
        double work = (G * mass_central * mass_object / 2.0) * (1.0 / r_initial - 1.0 / r_final);
        return format_scientific_3(work);
    }

    static void test_orbital_transfer_work() {
        if (!(orbital_transfer_work(5.972 * pow10(24), 1000.0, 6.371 * pow10(6), 7.0 * pow10(6)).equals("2.811e+09"))) {
            throw new RuntimeException(String.valueOf("case1 failed"));
        }
        if (!(orbital_transfer_work(5.972 * pow10(24), 500.0, 7.0 * pow10(6), 6.371 * pow10(6)).equals("-1.405e+09"))) {
            throw new RuntimeException(String.valueOf("case2 failed"));
        }
        if (!(orbital_transfer_work(1.989 * pow10(30), 1000.0, 1.5 * pow10(11), 2.28 * pow10(11)).equals("1.514e+11"))) {
            throw new RuntimeException(String.valueOf("case3 failed"));
        }
    }

    static void main() {
        test_orbital_transfer_work();
        System.out.println(orbital_transfer_work(5.972 * pow10(24), 1000.0, 6.371 * pow10(6), 7.0 * pow10(6)));
    }
    public static void main(String[] args) {
        main();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
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
