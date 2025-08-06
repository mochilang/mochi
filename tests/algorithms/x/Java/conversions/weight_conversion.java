public class Main {
    static java.util.Map<String,Double> KILOGRAM_CHART;
    static java.util.Map<String,Double> WEIGHT_TYPE_CHART;

    static double pow10(int exp) {
        double result = 1.0;
        if (exp >= 0) {
            int i = 0;
            while (i < exp) {
                result = result * 10.0;
                i = i + 1;
            }
        } else {
            int i_1 = 0;
            while (i_1 < (0 - exp)) {
                result = result / 10.0;
                i_1 = i_1 + 1;
            }
        }
        return result;
    }

    static double weight_conversion(String from_type, String to_type, double value) {
        Object has_to = KILOGRAM_CHART.containsKey(to_type);
        Object has_from = WEIGHT_TYPE_CHART.containsKey(from_type);
        if (((Boolean)(has_to)) && ((Boolean)(has_from))) {
            return value * (double)(((double)(KILOGRAM_CHART).getOrDefault(to_type, 0.0))) * (double)(((double)(WEIGHT_TYPE_CHART).getOrDefault(from_type, 0.0)));
        }
        System.out.println("Invalid 'from_type' or 'to_type'");
        return 0.0;
    }
    public static void main(String[] args) {
        KILOGRAM_CHART = ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("kilogram", 1.0), java.util.Map.entry("gram", 1000.0), java.util.Map.entry("milligram", 1000000.0), java.util.Map.entry("metric-ton", 0.001), java.util.Map.entry("long-ton", 0.0009842073), java.util.Map.entry("short-ton", 0.0011023122), java.util.Map.entry("pound", 2.2046244202), java.util.Map.entry("stone", 0.1574731728), java.util.Map.entry("ounce", 35.273990723), java.util.Map.entry("carrat", 5000.0), java.util.Map.entry("atomic-mass-unit", 6.022136652 * pow10(26))))));
        WEIGHT_TYPE_CHART = ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("kilogram", 1.0), java.util.Map.entry("gram", 0.001), java.util.Map.entry("milligram", 1e-06), java.util.Map.entry("metric-ton", 1000.0), java.util.Map.entry("long-ton", 1016.04608), java.util.Map.entry("short-ton", 907.184), java.util.Map.entry("pound", 0.453592), java.util.Map.entry("stone", 6.35029), java.util.Map.entry("ounce", 0.0283495), java.util.Map.entry("carrat", 0.0002), java.util.Map.entry("atomic-mass-unit", 1.660540199 * pow10(-27))))));
        System.out.println(weight_conversion("kilogram", "gram", 1.0));
        System.out.println(weight_conversion("gram", "pound", 3.0));
        System.out.println(weight_conversion("ounce", "kilogram", 3.0));
    }
}
