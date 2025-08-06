public class Main {

    static int round_to_int(double x) {
        if (x >= 0.0) {
            return ((Number)(x + 0.5)).intValue();
        }
        return ((Number)(x - 0.5)).intValue();
    }

    static int molarity_to_normality(double nfactor, double moles, double volume) {
        return round_to_int((moles / volume) * nfactor);
    }

    static int moles_to_pressure(double volume, double moles, double temperature) {
        return round_to_int((moles * 0.0821 * temperature) / volume);
    }

    static int moles_to_volume(double pressure, double moles, double temperature) {
        return round_to_int((moles * 0.0821 * temperature) / pressure);
    }

    static int pressure_and_volume_to_temperature(double pressure, double moles, double volume) {
        return round_to_int((pressure * volume) / (0.0821 * moles));
    }
    public static void main(String[] args) {
        System.out.println(_p(molarity_to_normality(2.0, 3.1, 0.31)));
        System.out.println(_p(molarity_to_normality(4.0, 11.4, 5.7)));
        System.out.println(_p(moles_to_pressure(0.82, 3.0, 300.0)));
        System.out.println(_p(moles_to_pressure(8.2, 5.0, 200.0)));
        System.out.println(_p(moles_to_volume(0.82, 3.0, 300.0)));
        System.out.println(_p(moles_to_volume(8.2, 5.0, 200.0)));
        System.out.println(_p(pressure_and_volume_to_temperature(0.82, 1.0, 2.0)));
        System.out.println(_p(pressure_and_volume_to_temperature(8.2, 5.0, 3.0)));
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
