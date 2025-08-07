public class Main {
    static double PI;
    static double TWO_PI;

    static double _mod(double x, double m) {
        return x - (((Number)(((Number)(x / m)).intValue())).doubleValue()) * m;
    }

    static double cos(double x) {
        double y = _mod(x + PI, TWO_PI) - PI;
        double y2 = y * y;
        double y4 = y2 * y2;
        double y6 = y4 * y2;
        return 1.0 - y2 / 2.0 + y4 / 24.0 - y6 / 720.0;
    }

    static double radians(double deg) {
        return deg * PI / 180.0;
    }

    static double abs_val(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double malus_law(double initial_intensity, double angle) {
        if (initial_intensity < 0.0) {
            throw new RuntimeException(String.valueOf("The value of intensity cannot be negative"));
        }
        if (angle < 0.0 || angle > 360.0) {
            throw new RuntimeException(String.valueOf("In Malus Law, the angle is in the range 0-360 degrees"));
        }
        double theta = radians(angle);
        double c = cos(theta);
        return initial_intensity * (c * c);
    }

    static void test_malus_law() {
        if (abs_val(malus_law(10.0, 45.0) - 5.0) > 0.01) {
            throw new RuntimeException(String.valueOf("malus_law test1 failed"));
        }
        if (abs_val(malus_law(100.0, 60.0) - 25.0) > 0.01) {
            throw new RuntimeException(String.valueOf("malus_law test2 failed"));
        }
        if (abs_val(malus_law(50.0, 150.0) - 37.5) > 0.01) {
            throw new RuntimeException(String.valueOf("malus_law test3 failed"));
        }
        if (abs_val(malus_law(75.0, 270.0) - 0.0) > 0.01) {
            throw new RuntimeException(String.valueOf("malus_law test4 failed"));
        }
        if (abs_val(malus_law(100.0, 180.0) - 100.0) > 0.01) {
            throw new RuntimeException(String.valueOf("malus_law test5 failed"));
        }
        if (abs_val(malus_law(100.0, 360.0) - 100.0) > 0.01) {
            throw new RuntimeException(String.valueOf("malus_law test6 failed"));
        }
    }

    static void main() {
        test_malus_law();
        System.out.println(_p(malus_law(100.0, 60.0)));
    }
    public static void main(String[] args) {
        PI = 3.141592653589793;
        TWO_PI = 6.283185307179586;
        main();
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
