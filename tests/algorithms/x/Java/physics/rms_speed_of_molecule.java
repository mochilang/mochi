public class Main {
    static double UNIVERSAL_GAS_CONSTANT;
    static double vrms;

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

    static double rms_speed_of_molecule(double temperature, double molar_mass) {
        if (temperature < 0.0) {
            throw new RuntimeException(String.valueOf("Temperature cannot be less than 0 K"));
        }
        if (molar_mass <= 0.0) {
            throw new RuntimeException(String.valueOf("Molar mass cannot be less than or equal to 0 kg/mol"));
        }
        double num = 3.0 * UNIVERSAL_GAS_CONSTANT * temperature;
        double val = num / molar_mass;
        double result = sqrt(val);
        return result;
    }
    public static void main(String[] args) {
        UNIVERSAL_GAS_CONSTANT = 8.3144598;
        System.out.println("rms_speed_of_molecule(100, 2) = " + _p(rms_speed_of_molecule(100.0, 2.0)));
        System.out.println("rms_speed_of_molecule(273, 12) = " + _p(rms_speed_of_molecule(273.0, 12.0)));
        vrms = rms_speed_of_molecule(300.0, 28.0);
        System.out.println("Vrms of Nitrogen gas at 300 K is " + _p(vrms) + " m/s");
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
