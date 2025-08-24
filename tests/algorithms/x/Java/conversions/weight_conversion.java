public class Main {
    static java.util.Map<String,Double> KILOGRAM_CHART;
    static java.util.Map<String,Double> WEIGHT_TYPE_CHART;

    static double pow10(java.math.BigInteger exp) {
        double result = (double)(1.0);
        if (exp.compareTo(java.math.BigInteger.valueOf(0)) >= 0) {
            java.math.BigInteger i_2 = java.math.BigInteger.valueOf(0);
            while (i_2.compareTo(exp) < 0) {
                result = (double)((double)(result) * (double)(10.0));
                i_2 = new java.math.BigInteger(String.valueOf(i_2.add(java.math.BigInteger.valueOf(1))));
            }
        } else {
            java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
            while (i_3.compareTo(((exp).negate())) < 0) {
                result = (double)((double)(result) / (double)(10.0));
                i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
            }
        }
        return result;
    }

    static double weight_conversion(String from_type, String to_type, double value) {
        boolean has_to = KILOGRAM_CHART.containsKey(to_type);
        boolean has_from_1 = WEIGHT_TYPE_CHART.containsKey(from_type);
        if (has_to && has_from_1) {
            return (double)((double)(value) * (double)(((double)(KILOGRAM_CHART).getOrDefault(to_type, 0.0)))) * (double)(((double)(WEIGHT_TYPE_CHART).getOrDefault(from_type, 0.0)));
        }
        System.out.println("Invalid 'from_type' or 'to_type'");
        return 0.0;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            KILOGRAM_CHART = ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("kilogram", (double)(1.0)), java.util.Map.entry("gram", (double)(1000.0)), java.util.Map.entry("milligram", (double)(1000000.0)), java.util.Map.entry("metric-ton", (double)(0.001)), java.util.Map.entry("long-ton", (double)(0.0009842073)), java.util.Map.entry("short-ton", (double)(0.0011023122)), java.util.Map.entry("pound", (double)(2.2046244202)), java.util.Map.entry("stone", (double)(0.1574731728)), java.util.Map.entry("ounce", (double)(35.273990723)), java.util.Map.entry("carrat", (double)(5000.0)), java.util.Map.entry("atomic-mass-unit", (double)((double)(6.022136652) * (double)(pow10(java.math.BigInteger.valueOf(26)))))))));
            WEIGHT_TYPE_CHART = ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("kilogram", (double)(1.0)), java.util.Map.entry("gram", (double)(0.001)), java.util.Map.entry("milligram", (double)(1e-06)), java.util.Map.entry("metric-ton", (double)(1000.0)), java.util.Map.entry("long-ton", (double)(1016.04608)), java.util.Map.entry("short-ton", (double)(907.184)), java.util.Map.entry("pound", (double)(0.453592)), java.util.Map.entry("stone", (double)(6.35029)), java.util.Map.entry("ounce", (double)(0.0283495)), java.util.Map.entry("carrat", (double)(0.0002)), java.util.Map.entry("atomic-mass-unit", (double)((double)(1.660540199) * (double)(pow10(new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(27)).negate()))))))))));
            System.out.println(weight_conversion("kilogram", "gram", (double)(1.0)));
            System.out.println(weight_conversion("gram", "pound", (double)(3.0)));
            System.out.println(weight_conversion("ounce", "kilogram", (double)(3.0)));
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }
}
