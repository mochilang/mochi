public class Main {
    static java.util.Map<String,String> UNIT_SYMBOL;
    static java.util.Map<String,Integer> METRIC_CONVERSION;
    static String ABBREVIATIONS;

    static String sanitize(String unit) {
        String res = unit.toLowerCase();
        if (_runeLen(res) > 0) {
            String last = _substr(res, _runeLen(res) - 1, _runeLen(res));
            if ((last.equals("s"))) {
                res = _substr(res, 0, _runeLen(res) - 1);
            }
        }
        if (((Boolean)(UNIT_SYMBOL.containsKey(res)))) {
            return ((String)(UNIT_SYMBOL).get(res));
        }
        return res;
    }

    static double pow10(int exp) {
        if (exp == 0) {
            return 1.0;
        }
        int e = exp;
        double res_1 = 1.0;
        if (e < 0) {
            e = -e;
        }
        int i = 0;
        while (i < e) {
            res_1 = res_1 * 10.0;
            i = i + 1;
        }
        if (exp < 0) {
            return 1.0 / res_1;
        }
        return res_1;
    }

    static double length_conversion(double value, String from_type, String to_type) {
        String from_sanitized = String.valueOf(sanitize(from_type));
        String to_sanitized = String.valueOf(sanitize(to_type));
        if (!(Boolean)(METRIC_CONVERSION.containsKey(from_sanitized))) {
            throw new RuntimeException(String.valueOf("Invalid 'from_type' value: '" + from_type + "'.\nConversion abbreviations are: " + ABBREVIATIONS));
        }
        if (!(Boolean)(METRIC_CONVERSION.containsKey(to_sanitized))) {
            throw new RuntimeException(String.valueOf("Invalid 'to_type' value: '" + to_type + "'.\nConversion abbreviations are: " + ABBREVIATIONS));
        }
        int from_exp = (int)(((int)(METRIC_CONVERSION).getOrDefault(from_sanitized, 0)));
        int to_exp = (int)(((int)(METRIC_CONVERSION).getOrDefault(to_sanitized, 0)));
        int exponent = 0;
        if (from_exp > to_exp) {
            exponent = from_exp - to_exp;
        } else {
            exponent = -(to_exp - from_exp);
        }
        return value * pow10(exponent);
    }
    public static void main(String[] args) {
        UNIT_SYMBOL = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>(java.util.Map.ofEntries(java.util.Map.entry("meter", "m"), java.util.Map.entry("kilometer", "km"), java.util.Map.entry("megametre", "Mm"), java.util.Map.entry("gigametre", "Gm"), java.util.Map.entry("terametre", "Tm"), java.util.Map.entry("petametre", "Pm"), java.util.Map.entry("exametre", "Em"), java.util.Map.entry("zettametre", "Zm"), java.util.Map.entry("yottametre", "Ym")))));
        METRIC_CONVERSION = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("m", 0), java.util.Map.entry("km", 3), java.util.Map.entry("Mm", 6), java.util.Map.entry("Gm", 9), java.util.Map.entry("Tm", 12), java.util.Map.entry("Pm", 15), java.util.Map.entry("Em", 18), java.util.Map.entry("Zm", 21), java.util.Map.entry("Ym", 24)))));
        ABBREVIATIONS = "m, km, Mm, Gm, Tm, Pm, Em, Zm, Ym";
        System.out.println(_p(length_conversion(1.0, "meter", "kilometer")));
        System.out.println(_p(length_conversion(1.0, "meter", "megametre")));
        System.out.println(_p(length_conversion(1.0, "gigametre", "meter")));
        System.out.println(_p(length_conversion(1.0, "terametre", "zettametre")));
        System.out.println(_p(length_conversion(1.0, "yottametre", "zettametre")));
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
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
