public class Main {
    static double C;

    static double energy_from_mass(double mass) {
        if (mass < 0.0) {
            throw new RuntimeException(String.valueOf("Mass can't be negative."));
        }
        return mass * C * C;
    }

    static double mass_from_energy(double energy) {
        if (energy < 0.0) {
            throw new RuntimeException(String.valueOf("Energy can't be negative."));
        }
        return energy / (C * C);
    }
    public static void main(String[] args) {
        C = 299792458.0;
        System.out.println(_p(energy_from_mass(124.56)));
        System.out.println(_p(energy_from_mass(320.0)));
        System.out.println(_p(energy_from_mass(0.0)));
        System.out.println(_p(mass_from_energy(124.56)));
        System.out.println(_p(mass_from_energy(320.0)));
        System.out.println(_p(mass_from_energy(0.0)));
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
