public class Main {
    static double PI;
    static double G;

    static double sqrt(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess = x;
        int i = 0;
        while (i < 10) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static double period_of_pendulum(double length) {
        if (length < 0.0) {
            throw new RuntimeException(String.valueOf("The length should be non-negative"));
        }
        return 2.0 * PI * sqrt(length / G);
    }
    public static void main(String[] args) {
        PI = 3.141592653589793;
        G = 9.80665;
        System.out.println(_p(period_of_pendulum(1.23)));
        System.out.println(_p(period_of_pendulum(2.37)));
        System.out.println(_p(period_of_pendulum(5.63)));
        System.out.println(_p(period_of_pendulum(0.0)));
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
