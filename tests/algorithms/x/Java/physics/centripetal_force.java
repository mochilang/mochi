public class Main {

    static double centripetal(double mass, double velocity, double radius) {
        if (mass < 0.0) {
            throw new RuntimeException(String.valueOf("The mass of the body cannot be negative"));
        }
        if (radius <= 0.0) {
            throw new RuntimeException(String.valueOf("The radius is always a positive non zero integer"));
        }
        return mass * velocity * velocity / radius;
    }

    static double floor(double x) {
        int i = ((Number)(x)).intValue();
        if ((((Number)(i)).doubleValue()) > x) {
            i = i - 1;
        }
        return ((Number)(i)).doubleValue();
    }

    static double pow10(int n) {
        double p = 1.0;
        int i_1 = 0;
        while (i_1 < n) {
            p = p * 10.0;
            i_1 = i_1 + 1;
        }
        return p;
    }

    static double round(double x, int n) {
        double m = pow10(n);
        return floor(x * m + 0.5) / m;
    }

    static void show(double mass, double velocity, double radius) {
        double f = centripetal(mass, velocity, radius);
        System.out.println(_p(round(f, 2)));
    }
    public static void main(String[] args) {
        show(15.5, -30.0, 10.0);
        show(10.0, 15.0, 5.0);
        show(20.0, -50.0, 15.0);
        show(12.25, 40.0, 25.0);
        show(50.0, 100.0, 50.0);
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
