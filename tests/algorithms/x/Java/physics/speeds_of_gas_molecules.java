public class Main {
    static double PI;
    static double R;

    static double sqrt(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess = x;
        int i = 0;
        while (i < 20) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static double avg_speed_of_molecule(double temperature, double molar_mass) {
        if (temperature < 0.0) {
            throw new RuntimeException(String.valueOf("Absolute temperature cannot be less than 0 K"));
        }
        if (molar_mass <= 0.0) {
            throw new RuntimeException(String.valueOf("Molar mass should be greater than 0 kg/mol"));
        }
        double expr = 8.0 * R * temperature / (PI * molar_mass);
        double s = sqrt(expr);
        return s;
    }

    static double mps_speed_of_molecule(double temperature, double molar_mass) {
        if (temperature < 0.0) {
            throw new RuntimeException(String.valueOf("Absolute temperature cannot be less than 0 K"));
        }
        if (molar_mass <= 0.0) {
            throw new RuntimeException(String.valueOf("Molar mass should be greater than 0 kg/mol"));
        }
        double expr_1 = 2.0 * R * temperature / molar_mass;
        double s_1 = sqrt(expr_1);
        return s_1;
    }
    public static void main(String[] args) {
        PI = 3.141592653589793;
        R = 8.31446261815324;
        System.out.println(_p(avg_speed_of_molecule(273.0, 0.028)));
        System.out.println(_p(avg_speed_of_molecule(300.0, 0.032)));
        System.out.println(_p(mps_speed_of_molecule(273.0, 0.028)));
        System.out.println(_p(mps_speed_of_molecule(300.0, 0.032)));
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
