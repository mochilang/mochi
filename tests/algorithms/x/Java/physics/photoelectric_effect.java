public class Main {
    static double PLANCK_CONSTANT_JS;
    static double PLANCK_CONSTANT_EVS;

    static double pow10(int exp) {
        double result = 1.0;
        int i = 0;
        while (i < exp) {
            result = result * 10.0;
            i = i + 1;
        }
        return result;
    }

    static double maximum_kinetic_energy(double frequency, double work_function, boolean in_ev) {
        if (frequency < 0.0) {
            throw new RuntimeException(String.valueOf("Frequency can't be negative."));
        }
        double energy = in_ev ? PLANCK_CONSTANT_EVS * frequency - work_function : PLANCK_CONSTANT_JS * frequency - work_function;
        if (energy > 0.0) {
            return energy;
        }
        return 0.0;
    }
    public static void main(String[] args) {
        PLANCK_CONSTANT_JS = 6.6261 / pow10(34);
        PLANCK_CONSTANT_EVS = 4.1357 / pow10(15);
        System.out.println(_p(maximum_kinetic_energy(1000000.0, 2.0, false)));
        System.out.println(_p(maximum_kinetic_energy(1000000.0, 2.0, true)));
        System.out.println(_p(maximum_kinetic_energy(10000000000000000.0, 2.0, true)));
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
