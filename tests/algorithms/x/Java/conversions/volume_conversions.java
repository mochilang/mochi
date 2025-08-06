public class Main {
    static String[] units;
    static double[] from_factors;
    static double[] to_factors;

    static String supported_values() {
        String result = units[0];
        int i = 1;
        while (i < units.length) {
            result = result + ", " + units[i];
            i = i + 1;
        }
        return result;
    }

    static int find_index(String name) {
        int i_1 = 0;
        while (i_1 < units.length) {
            if ((units[i_1].equals(name))) {
                return i_1;
            }
            i_1 = i_1 + 1;
        }
        return -1;
    }

    static double get_from_factor(String name) {
        int idx = find_index(name);
        if (idx < 0) {
            throw new RuntimeException(String.valueOf("Invalid 'from_type' value: '" + name + "' Supported values are: " + String.valueOf(supported_values())));
        }
        return from_factors[idx];
    }

    static double get_to_factor(String name) {
        int idx_1 = find_index(name);
        if (idx_1 < 0) {
            throw new RuntimeException(String.valueOf("Invalid 'to_type' value: '" + name + "' Supported values are: " + String.valueOf(supported_values())));
        }
        return to_factors[idx_1];
    }

    static double volume_conversion(double value, String from_type, String to_type) {
        double from_factor = get_from_factor(from_type);
        double to_factor = get_to_factor(to_type);
        return value * from_factor * to_factor;
    }
    public static void main(String[] args) {
        units = ((String[])(new String[]{"cubic meter", "litre", "kilolitre", "gallon", "cubic yard", "cubic foot", "cup"}));
        from_factors = ((double[])(new double[]{1.0, 0.001, 1.0, 0.00454, 0.76455, 0.028, 0.000236588}));
        to_factors = ((double[])(new double[]{1.0, 1000.0, 1.0, 264.172, 1.30795, 35.3147, 4226.75}));
        System.out.println(_p(volume_conversion(4.0, "cubic meter", "litre")));
        System.out.println(_p(volume_conversion(1.0, "litre", "gallon")));
        System.out.println(_p(volume_conversion(1.0, "kilolitre", "cubic meter")));
        System.out.println(_p(volume_conversion(3.0, "gallon", "cubic yard")));
        System.out.println(_p(volume_conversion(2.0, "cubic yard", "litre")));
        System.out.println(_p(volume_conversion(4.0, "cubic foot", "cup")));
        System.out.println(_p(volume_conversion(1.0, "cup", "kilolitre")));
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
