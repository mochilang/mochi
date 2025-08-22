public class Main {
    static double C = (double)(299792458.0);

    static double energy_from_mass(double mass) {
        if ((double)(mass) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Mass can't be negative."));
        }
        return (double)((double)(mass) * (double)(C)) * (double)(C);
    }

    static double mass_from_energy(double energy) {
        if ((double)(energy) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Energy can't be negative."));
        }
        return (double)(energy) / (double)(((double)(C) * (double)(C)));
    }
    public static void main(String[] args) {
        System.out.println(_p(energy_from_mass((double)(124.56))));
        System.out.println(_p(energy_from_mass((double)(320.0))));
        System.out.println(_p(energy_from_mass((double)(0.0))));
        System.out.println(_p(mass_from_energy((double)(124.56))));
        System.out.println(_p(mass_from_energy((double)(320.0))));
        System.out.println(_p(mass_from_energy((double)(0.0))));
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
