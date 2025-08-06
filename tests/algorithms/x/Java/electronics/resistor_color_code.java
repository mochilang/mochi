public class Main {
    static String[] valid_colors;
    static java.util.Map<String,Integer> significant_figures_color_values;
    static java.util.Map<String,Double> multiplier_color_values;
    static java.util.Map<String,Double> tolerance_color_values;
    static java.util.Map<String,Integer> temperature_coeffecient_color_values;

    static boolean contains(String[] list, String value) {
        for (String c : list) {
            if ((c.equals(value))) {
                return true;
            }
        }
        return false;
    }

    static int get_significant_digits(String[] colors) {
        int digit = 0;
        for (String color : colors) {
            if (!(Boolean)(significant_figures_color_values.containsKey(color))) {
                throw new RuntimeException(String.valueOf(color + " is not a valid color for significant figure bands"));
            }
            digit = digit * 10 + (int)(((int)(significant_figures_color_values).getOrDefault(color, 0)));
        }
        return digit;
    }

    static double get_multiplier(String color) {
        if (!(Boolean)(multiplier_color_values.containsKey(color))) {
            throw new RuntimeException(String.valueOf(color + " is not a valid color for multiplier band"));
        }
        return ((double)(multiplier_color_values).getOrDefault(color, 0.0));
    }

    static double get_tolerance(String color) {
        if (!(Boolean)(tolerance_color_values.containsKey(color))) {
            throw new RuntimeException(String.valueOf(color + " is not a valid color for tolerance band"));
        }
        return ((double)(tolerance_color_values).getOrDefault(color, 0.0));
    }

    static int get_temperature_coeffecient(String color) {
        if (!(Boolean)(temperature_coeffecient_color_values.containsKey(color))) {
            throw new RuntimeException(String.valueOf(color + " is not a valid color for temperature coeffecient band"));
        }
        return ((int)(temperature_coeffecient_color_values).getOrDefault(color, 0));
    }

    static int get_band_type_count(int total, String typ) {
        if (total == 3) {
            if ((typ.equals("significant"))) {
                return 2;
            }
            if ((typ.equals("multiplier"))) {
                return 1;
            }
            throw new RuntimeException(String.valueOf(typ + " is not valid for a 3 band resistor"));
        } else         if (total == 4) {
            if ((typ.equals("significant"))) {
                return 2;
            }
            if ((typ.equals("multiplier"))) {
                return 1;
            }
            if ((typ.equals("tolerance"))) {
                return 1;
            }
            throw new RuntimeException(String.valueOf(typ + " is not valid for a 4 band resistor"));
        } else         if (total == 5) {
            if ((typ.equals("significant"))) {
                return 3;
            }
            if ((typ.equals("multiplier"))) {
                return 1;
            }
            if ((typ.equals("tolerance"))) {
                return 1;
            }
            throw new RuntimeException(String.valueOf(typ + " is not valid for a 5 band resistor"));
        } else         if (total == 6) {
            if ((typ.equals("significant"))) {
                return 3;
            }
            if ((typ.equals("multiplier"))) {
                return 1;
            }
            if ((typ.equals("tolerance"))) {
                return 1;
            }
            if ((typ.equals("temp_coeffecient"))) {
                return 1;
            }
            throw new RuntimeException(String.valueOf(typ + " is not valid for a 6 band resistor"));
        } else {
            throw new RuntimeException(String.valueOf(_p(total) + " is not a valid number of bands"));
        }
    }

    static boolean check_validity(int number_of_bands, String[] colors) {
        if (number_of_bands < 3 || number_of_bands > 6) {
            throw new RuntimeException(String.valueOf("Invalid number of bands. Resistor bands must be 3 to 6"));
        }
        if (number_of_bands != colors.length) {
            throw new RuntimeException(String.valueOf("Expecting " + _p(number_of_bands) + " colors, provided " + _p(colors.length) + " colors"));
        }
        for (String color : colors) {
            if (!(Boolean)contains(((String[])(valid_colors)), color)) {
                throw new RuntimeException(String.valueOf(color + " is not a valid color"));
            }
        }
        return true;
    }

    static String calculate_resistance(int number_of_bands, String[] color_code_list) {
        check_validity(number_of_bands, ((String[])(color_code_list)));
        int sig_count = get_band_type_count(number_of_bands, "significant");
        String[] significant_colors = ((String[])(java.util.Arrays.copyOfRange(color_code_list, 0, sig_count)));
        int significant_digits = get_significant_digits(((String[])(significant_colors)));
        String multiplier_color = color_code_list[sig_count];
        double multiplier = get_multiplier(multiplier_color);
        double tolerance = 20.0;
        if (number_of_bands >= 4) {
            String tolerance_color = color_code_list[sig_count + 1];
            tolerance = get_tolerance(tolerance_color);
        }
        int temp_coeff = 0;
        if (number_of_bands == 6) {
            String temp_color = color_code_list[sig_count + 2];
            temp_coeff = get_temperature_coeffecient(temp_color);
        }
        double resistance_value = multiplier * significant_digits;
        String resistance_str = _p(resistance_value);
        if (resistance_value == ((Number)(resistance_value)).intValue()) {
            resistance_str = _p(((Number)(resistance_value)).intValue());
        }
        String answer = resistance_str + "Ω ±" + _p(tolerance) + "% ";
        if (temp_coeff != 0) {
            answer = answer + _p(temp_coeff) + " ppm/K";
        }
        return answer;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            valid_colors = ((String[])(new String[]{"Black", "Brown", "Red", "Orange", "Yellow", "Green", "Blue", "Violet", "Grey", "White", "Gold", "Silver"}));
            significant_figures_color_values = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("Black", 0), java.util.Map.entry("Brown", 1), java.util.Map.entry("Red", 2), java.util.Map.entry("Orange", 3), java.util.Map.entry("Yellow", 4), java.util.Map.entry("Green", 5), java.util.Map.entry("Blue", 6), java.util.Map.entry("Violet", 7), java.util.Map.entry("Grey", 8), java.util.Map.entry("White", 9)))));
            multiplier_color_values = ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("Black", 1.0), java.util.Map.entry("Brown", 10.0), java.util.Map.entry("Red", 100.0), java.util.Map.entry("Orange", 1000.0), java.util.Map.entry("Yellow", 10000.0), java.util.Map.entry("Green", 100000.0), java.util.Map.entry("Blue", 1000000.0), java.util.Map.entry("Violet", 10000000.0), java.util.Map.entry("Grey", 100000000.0), java.util.Map.entry("White", 1000000000.0), java.util.Map.entry("Gold", 0.1), java.util.Map.entry("Silver", 0.01)))));
            tolerance_color_values = ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("Brown", 1.0), java.util.Map.entry("Red", 2.0), java.util.Map.entry("Orange", 0.05), java.util.Map.entry("Yellow", 0.02), java.util.Map.entry("Green", 0.5), java.util.Map.entry("Blue", 0.25), java.util.Map.entry("Violet", 0.1), java.util.Map.entry("Grey", 0.01), java.util.Map.entry("Gold", 5.0), java.util.Map.entry("Silver", 10.0)))));
            temperature_coeffecient_color_values = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("Black", 250), java.util.Map.entry("Brown", 100), java.util.Map.entry("Red", 50), java.util.Map.entry("Orange", 15), java.util.Map.entry("Yellow", 25), java.util.Map.entry("Green", 20), java.util.Map.entry("Blue", 10), java.util.Map.entry("Violet", 5), java.util.Map.entry("Grey", 1)))));
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
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
