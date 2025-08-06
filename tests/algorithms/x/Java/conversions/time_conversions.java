public class Main {
    static java.util.Map<String,Double> time_chart;
    static java.util.Map<String,Double> time_chart_inverse;
    static String[] units;
    static String units_str;

    static boolean contains(String[] arr, String t) {
        int i = 0;
        while (i < arr.length) {
            if ((arr[i].equals(t))) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static double convert_time(double time_value, String unit_from, String unit_to) {
        if (time_value < 0.0) {
            throw new RuntimeException(String.valueOf("'time_value' must be a non-negative number."));
        }
        String from = unit_from.toLowerCase();
        String to = unit_to.toLowerCase();
        if ((!(Boolean)contains(((String[])(units)), from)) || (!(Boolean)contains(((String[])(units)), to))) {
            String invalid_unit = from;
            if (((Boolean)(contains(((String[])(units)), from)))) {
                invalid_unit = to;
            }
            throw new RuntimeException(String.valueOf("Invalid unit " + invalid_unit + " is not in " + units_str + "."));
        }
        double seconds = time_value * (double)(((double)(time_chart).getOrDefault(from, 0.0)));
        double converted = seconds * (double)(((double)(time_chart_inverse).getOrDefault(to, 0.0)));
        double scaled = converted * 1000.0;
        int int_part = ((Number)(scaled + 0.5)).intValue();
        return (int_part + 0.0) / 1000.0;
    }
    public static void main(String[] args) {
        time_chart = ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("seconds", 1.0), java.util.Map.entry("minutes", 60.0), java.util.Map.entry("hours", 3600.0), java.util.Map.entry("days", 86400.0), java.util.Map.entry("weeks", 604800.0), java.util.Map.entry("months", 2629800.0), java.util.Map.entry("years", 31557600.0)))));
        time_chart_inverse = ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("seconds", 1.0), java.util.Map.entry("minutes", 1.0 / 60.0), java.util.Map.entry("hours", 1.0 / 3600.0), java.util.Map.entry("days", 1.0 / 86400.0), java.util.Map.entry("weeks", 1.0 / 604800.0), java.util.Map.entry("months", 1.0 / 2629800.0), java.util.Map.entry("years", 1.0 / 31557600.0)))));
        units = ((String[])(new String[]{"seconds", "minutes", "hours", "days", "weeks", "months", "years"}));
        units_str = "seconds, minutes, hours, days, weeks, months, years";
        System.out.println(convert_time(3600.0, "seconds", "hours"));
        System.out.println(convert_time(360.0, "days", "months"));
        System.out.println(convert_time(360.0, "months", "years"));
        System.out.println(convert_time(1.0, "years", "seconds"));
    }
}
