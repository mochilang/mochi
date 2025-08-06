public class Main {
    static java.util.Map<String,Integer> SI_UNITS;
    static java.util.Map<String,Integer> BINARY_UNITS;

    static double pow(double base, int exp) {
        if (exp == 0) {
            return 1.0;
        }
        int e = exp;
        if (e < 0) {
            e = -e;
        }
        double result = 1.0;
        int i = 0;
        while (i < e) {
            result = result * base;
            i = i + 1;
        }
        if (exp < 0) {
            return 1.0 / result;
        }
        return result;
    }

    static double convert_si_prefix(double known_amount, String known_prefix, String unknown_prefix) {
        String kp = known_prefix.toLowerCase();
        String up = unknown_prefix.toLowerCase();
        if (!(Boolean)(SI_UNITS.containsKey(kp))) {
            throw new RuntimeException(String.valueOf("unknown prefix: " + known_prefix));
        }
        if (!(Boolean)(SI_UNITS.containsKey(up))) {
            throw new RuntimeException(String.valueOf("unknown prefix: " + unknown_prefix));
        }
        int diff = (int)(((int)(SI_UNITS).getOrDefault(kp, 0))) - (int)(((int)(SI_UNITS).getOrDefault(up, 0)));
        return known_amount * pow(10.0, diff);
    }

    static double convert_binary_prefix(double known_amount, String known_prefix, String unknown_prefix) {
        String kp_1 = known_prefix.toLowerCase();
        String up_1 = unknown_prefix.toLowerCase();
        if (!(Boolean)(BINARY_UNITS.containsKey(kp_1))) {
            throw new RuntimeException(String.valueOf("unknown prefix: " + known_prefix));
        }
        if (!(Boolean)(BINARY_UNITS.containsKey(up_1))) {
            throw new RuntimeException(String.valueOf("unknown prefix: " + unknown_prefix));
        }
        int diff_1 = ((int)(((int)(BINARY_UNITS).getOrDefault(kp_1, 0))) - (int)(((int)(BINARY_UNITS).getOrDefault(up_1, 0)))) * 10;
        return known_amount * pow(2.0, diff_1);
    }
    public static void main(String[] args) {
        SI_UNITS = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("yotta", 24), java.util.Map.entry("zetta", 21), java.util.Map.entry("exa", 18), java.util.Map.entry("peta", 15), java.util.Map.entry("tera", 12), java.util.Map.entry("giga", 9), java.util.Map.entry("mega", 6), java.util.Map.entry("kilo", 3), java.util.Map.entry("hecto", 2), java.util.Map.entry("deca", 1), java.util.Map.entry("deci", -1), java.util.Map.entry("centi", -2), java.util.Map.entry("milli", -3), java.util.Map.entry("micro", -6), java.util.Map.entry("nano", -9), java.util.Map.entry("pico", -12), java.util.Map.entry("femto", -15), java.util.Map.entry("atto", -18), java.util.Map.entry("zepto", -21), java.util.Map.entry("yocto", -24)))));
        BINARY_UNITS = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("yotta", 8), java.util.Map.entry("zetta", 7), java.util.Map.entry("exa", 6), java.util.Map.entry("peta", 5), java.util.Map.entry("tera", 4), java.util.Map.entry("giga", 3), java.util.Map.entry("mega", 2), java.util.Map.entry("kilo", 1)))));
        System.out.println(_p(convert_si_prefix(1.0, "giga", "mega")));
        System.out.println(_p(convert_si_prefix(1.0, "mega", "giga")));
        System.out.println(_p(convert_si_prefix(1.0, "kilo", "kilo")));
        System.out.println(_p(convert_binary_prefix(1.0, "giga", "mega")));
        System.out.println(_p(convert_binary_prefix(1.0, "mega", "giga")));
        System.out.println(_p(convert_binary_prefix(1.0, "kilo", "kilo")));
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
