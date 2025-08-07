public class Main {
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

    static double terminal_velocity(double mass, double density, double area, double drag_coefficient) {
        if (mass <= 0.0 || density <= 0.0 || area <= 0.0 || drag_coefficient <= 0.0) {
            throw new RuntimeException(String.valueOf("mass, density, area and the drag coefficient all need to be positive"));
        }
        double numerator = 2.0 * mass * G;
        double denominator = density * area * drag_coefficient;
        double result = sqrt(numerator / denominator);
        return result;
    }
    public static void main(String[] args) {
        G = 9.80665;
        System.out.println(_p(terminal_velocity(1.0, 25.0, 0.6, 0.77)));
        System.out.println(_p(terminal_velocity(2.0, 100.0, 0.45, 0.23)));
        System.out.println(_p(terminal_velocity(5.0, 50.0, 0.2, 0.5)));
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
