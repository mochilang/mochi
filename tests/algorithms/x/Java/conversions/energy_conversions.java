public class Main {
    static java.util.Map<String,Double> ENERGY_CONVERSION;

    static double energy_conversion(String from_type, String to_type, double value) {
        if (((Number)((ENERGY_CONVERSION.containsKey(from_type)))).intValue() == false || ((Number)((ENERGY_CONVERSION.containsKey(to_type)))).intValue() == false) {
            throw new RuntimeException(String.valueOf("Incorrect 'from_type' or 'to_type'"));
        }
        return value * (double)(((double)(ENERGY_CONVERSION).getOrDefault(from_type, 0.0))) / (double)(((double)(ENERGY_CONVERSION).getOrDefault(to_type, 0.0)));
    }
    public static void main(String[] args) {
        ENERGY_CONVERSION = ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("joule", 1.0), java.util.Map.entry("kilojoule", 1000.0), java.util.Map.entry("megajoule", 1000000.0), java.util.Map.entry("gigajoule", 1000000000.0), java.util.Map.entry("wattsecond", 1.0), java.util.Map.entry("watthour", 3600.0), java.util.Map.entry("kilowatthour", 3600000.0), java.util.Map.entry("newtonmeter", 1.0), java.util.Map.entry("calorie_nutr", 4186.8), java.util.Map.entry("kilocalorie_nutr", 4186800.0), java.util.Map.entry("electronvolt", 1.602176634e-19), java.util.Map.entry("britishthermalunit_it", 1055.05585), java.util.Map.entry("footpound", 1.355818)))));
        System.out.println(_p(energy_conversion("joule", "kilojoule", 1.0)));
        System.out.println(_p(energy_conversion("kilowatthour", "joule", 10.0)));
        System.out.println(_p(energy_conversion("britishthermalunit_it", "footpound", 1.0)));
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
