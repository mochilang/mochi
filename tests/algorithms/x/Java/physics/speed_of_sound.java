public class Main {

    static double sqrtApprox(double x) {
        if (x == 0.0) {
            return 0.0;
        }
        double guess = x / 2.0;
        int i = 0;
        while (i < 20) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static double speed_of_sound_in_a_fluid(double density, double bulk_modulus) {
        if (density <= 0.0) {
            throw new RuntimeException(String.valueOf("Impossible fluid density"));
        }
        if (bulk_modulus <= 0.0) {
            throw new RuntimeException(String.valueOf("Impossible bulk modulus"));
        }
        return sqrtApprox(bulk_modulus / density);
    }
    public static void main(String[] args) {
        System.out.println(_p(speed_of_sound_in_a_fluid(998.0, 2150000000.0)));
        System.out.println(_p(speed_of_sound_in_a_fluid(13600.0, 28500000000.0)));
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
