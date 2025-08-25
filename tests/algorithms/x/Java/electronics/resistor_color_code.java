public class Main {
    static String[] valid_colors = ((String[])(new String[]{"Black", "Brown", "Red", "Orange", "Yellow", "Green", "Blue", "Violet", "Grey", "White", "Gold", "Silver"}));
    static java.util.Map<String,java.math.BigInteger> significant_figures_color_values;
    static java.util.Map<String,Double> multiplier_color_values;
    static java.util.Map<String,Double> tolerance_color_values;
    static java.util.Map<String,java.math.BigInteger> temperature_coeffecient_color_values;

    static boolean contains(String[] list, String value) {
        for (String c : list) {
            if ((c.equals(value))) {
                return true;
            }
        }
        return false;
    }

    static java.math.BigInteger get_significant_digits(String[] colors) {
        java.math.BigInteger digit = java.math.BigInteger.valueOf(0);
        for (String color : colors) {
            if (!(significant_figures_color_values.containsKey(color))) {
                throw new RuntimeException(String.valueOf(color + " is not a valid color for significant figure bands"));
            }
            digit = new java.math.BigInteger(String.valueOf(digit.multiply(java.math.BigInteger.valueOf(10)).add(((java.math.BigInteger)(significant_figures_color_values).get(color)))));
        }
        return new java.math.BigInteger(String.valueOf(digit));
    }

    static double get_multiplier(String color) {
        if (!(multiplier_color_values.containsKey(color))) {
            throw new RuntimeException(String.valueOf(color + " is not a valid color for multiplier band"));
        }
        return (double)(((double)(multiplier_color_values).getOrDefault(color, 0.0)));
    }

    static double get_tolerance(String color) {
        if (!(tolerance_color_values.containsKey(color))) {
            throw new RuntimeException(String.valueOf(color + " is not a valid color for tolerance band"));
        }
        return (double)(((double)(tolerance_color_values).getOrDefault(color, 0.0)));
    }

    static java.math.BigInteger get_temperature_coeffecient(String color) {
        if (!(temperature_coeffecient_color_values.containsKey(color))) {
            throw new RuntimeException(String.valueOf(color + " is not a valid color for temperature coeffecient band"));
        }
        return new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(temperature_coeffecient_color_values).get(color))));
    }

    static java.math.BigInteger get_band_type_count(java.math.BigInteger total, String typ) {
        if (total.compareTo(java.math.BigInteger.valueOf(3)) == 0) {
            if ((typ.equals("significant"))) {
                return java.math.BigInteger.valueOf(2);
            }
            if ((typ.equals("multiplier"))) {
                return java.math.BigInteger.valueOf(1);
            }
            throw new RuntimeException(String.valueOf(typ + " is not valid for a 3 band resistor"));
        } else         if (total.compareTo(java.math.BigInteger.valueOf(4)) == 0) {
            if ((typ.equals("significant"))) {
                return java.math.BigInteger.valueOf(2);
            }
            if ((typ.equals("multiplier"))) {
                return java.math.BigInteger.valueOf(1);
            }
            if ((typ.equals("tolerance"))) {
                return java.math.BigInteger.valueOf(1);
            }
            throw new RuntimeException(String.valueOf(typ + " is not valid for a 4 band resistor"));
        } else         if (total.compareTo(java.math.BigInteger.valueOf(5)) == 0) {
            if ((typ.equals("significant"))) {
                return java.math.BigInteger.valueOf(3);
            }
            if ((typ.equals("multiplier"))) {
                return java.math.BigInteger.valueOf(1);
            }
            if ((typ.equals("tolerance"))) {
                return java.math.BigInteger.valueOf(1);
            }
            throw new RuntimeException(String.valueOf(typ + " is not valid for a 5 band resistor"));
        } else         if (total.compareTo(java.math.BigInteger.valueOf(6)) == 0) {
            if ((typ.equals("significant"))) {
                return java.math.BigInteger.valueOf(3);
            }
            if ((typ.equals("multiplier"))) {
                return java.math.BigInteger.valueOf(1);
            }
            if ((typ.equals("tolerance"))) {
                return java.math.BigInteger.valueOf(1);
            }
            if ((typ.equals("temp_coeffecient"))) {
                return java.math.BigInteger.valueOf(1);
            }
            throw new RuntimeException(String.valueOf(typ + " is not valid for a 6 band resistor"));
        } else {
            throw new RuntimeException(String.valueOf(_p(total) + " is not a valid number of bands"));
        }
    }

    static boolean check_validity(java.math.BigInteger number_of_bands, String[] colors) {
        if (number_of_bands.compareTo(java.math.BigInteger.valueOf(3)) < 0 || number_of_bands.compareTo(java.math.BigInteger.valueOf(6)) > 0) {
            throw new RuntimeException(String.valueOf("Invalid number of bands. Resistor bands must be 3 to 6"));
        }
        if (number_of_bands.compareTo(new java.math.BigInteger(String.valueOf(colors.length))) != 0) {
            throw new RuntimeException(String.valueOf("Expecting " + _p(number_of_bands) + " colors, provided " + _p(colors.length) + " colors"));
        }
        for (String color : colors) {
            if (!(Boolean)contains(((String[])(valid_colors)), color)) {
                throw new RuntimeException(String.valueOf(color + " is not a valid color"));
            }
        }
        return true;
    }

    static String calculate_resistance(java.math.BigInteger number_of_bands, String[] color_code_list) {
        check_validity(new java.math.BigInteger(String.valueOf(number_of_bands)), ((String[])(color_code_list)));
        java.math.BigInteger sig_count_1 = new java.math.BigInteger(String.valueOf(get_band_type_count(new java.math.BigInteger(String.valueOf(number_of_bands)), "significant")));
        String[] significant_colors_1 = ((String[])(java.util.Arrays.copyOfRange(color_code_list, (int)(0L), (int)(((java.math.BigInteger)(sig_count_1)).longValue()))));
        java.math.BigInteger significant_digits_1 = new java.math.BigInteger(String.valueOf(get_significant_digits(((String[])(significant_colors_1)))));
        String multiplier_color_1 = color_code_list[_idx((color_code_list).length, ((java.math.BigInteger)(sig_count_1)).longValue())];
        double multiplier_1 = (double)(get_multiplier(multiplier_color_1));
        double tolerance_1 = (double)(20.0);
        if (number_of_bands.compareTo(java.math.BigInteger.valueOf(4)) >= 0) {
            String tolerance_color_1 = color_code_list[_idx((color_code_list).length, ((java.math.BigInteger)(sig_count_1.add(java.math.BigInteger.valueOf(1)))).longValue())];
            tolerance_1 = (double)(get_tolerance(tolerance_color_1));
        }
        java.math.BigInteger temp_coeff_1 = java.math.BigInteger.valueOf(0);
        if (number_of_bands.compareTo(java.math.BigInteger.valueOf(6)) == 0) {
            String temp_color_1 = color_code_list[_idx((color_code_list).length, ((java.math.BigInteger)(sig_count_1.add(java.math.BigInteger.valueOf(2)))).longValue())];
            temp_coeff_1 = new java.math.BigInteger(String.valueOf(get_temperature_coeffecient(temp_color_1)));
        }
        java.math.BigInteger resistance_value_1 = new java.math.BigInteger(String.valueOf(new java.math.BigInteger(String.valueOf(multiplier_1)).multiply(significant_digits_1)));
        String resistance_str_1 = _p(resistance_value_1);
        if (resistance_value_1.compareTo(new java.math.BigInteger(String.valueOf(((Number)(resistance_value_1)).intValue()))) == 0) {
            resistance_str_1 = _p(((Number)(resistance_value_1)).intValue());
        }
        String answer_1 = resistance_str_1 + "Ω ±" + _p(tolerance_1) + "% ";
        if (temp_coeff_1.compareTo(java.math.BigInteger.valueOf(0)) != 0) {
            answer_1 = answer_1 + _p(temp_coeff_1) + " ppm/K";
        }
        return answer_1;
    }
    public static void main(String[] args) {
        significant_figures_color_values = ((java.util.Map<String,java.math.BigInteger>)(new java.util.LinkedHashMap<String, java.math.BigInteger>() {{ put("Black", java.math.BigInteger.valueOf(0)); put("Brown", java.math.BigInteger.valueOf(1)); put("Red", java.math.BigInteger.valueOf(2)); put("Orange", java.math.BigInteger.valueOf(3)); put("Yellow", java.math.BigInteger.valueOf(4)); put("Green", java.math.BigInteger.valueOf(5)); put("Blue", java.math.BigInteger.valueOf(6)); put("Violet", java.math.BigInteger.valueOf(7)); put("Grey", java.math.BigInteger.valueOf(8)); put("White", java.math.BigInteger.valueOf(9)); }}));
        multiplier_color_values = ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>() {{ put("Black", (double)(1.0)); put("Brown", (double)(10.0)); put("Red", (double)(100.0)); put("Orange", (double)(1000.0)); put("Yellow", (double)(10000.0)); put("Green", (double)(100000.0)); put("Blue", (double)(1000000.0)); put("Violet", (double)(10000000.0)); put("Grey", (double)(100000000.0)); put("White", (double)(1000000000.0)); put("Gold", (double)(0.1)); put("Silver", (double)(0.01)); }}));
        tolerance_color_values = ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>() {{ put("Brown", (double)(1.0)); put("Red", (double)(2.0)); put("Orange", (double)(0.05)); put("Yellow", (double)(0.02)); put("Green", (double)(0.5)); put("Blue", (double)(0.25)); put("Violet", (double)(0.1)); put("Grey", (double)(0.01)); put("Gold", (double)(5.0)); put("Silver", (double)(10.0)); }}));
        temperature_coeffecient_color_values = ((java.util.Map<String,java.math.BigInteger>)(new java.util.LinkedHashMap<String, java.math.BigInteger>() {{ put("Black", java.math.BigInteger.valueOf(250)); put("Brown", java.math.BigInteger.valueOf(100)); put("Red", java.math.BigInteger.valueOf(50)); put("Orange", java.math.BigInteger.valueOf(15)); put("Yellow", java.math.BigInteger.valueOf(25)); put("Green", java.math.BigInteger.valueOf(20)); put("Blue", java.math.BigInteger.valueOf(10)); put("Violet", java.math.BigInteger.valueOf(5)); put("Grey", java.math.BigInteger.valueOf(1)); }}));
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
        if (v instanceof java.util.Map<?, ?>) {
            StringBuilder sb = new StringBuilder("{");
            boolean first = true;
            for (java.util.Map.Entry<?, ?> e : ((java.util.Map<?, ?>) v).entrySet()) {
                if (!first) sb.append(", ");
                sb.append(_p(e.getKey()));
                sb.append("=");
                sb.append(_p(e.getValue()));
                first = false;
            }
            sb.append("}");
            return sb.toString();
        }
        if (v instanceof java.util.List<?>) {
            StringBuilder sb = new StringBuilder("[");
            boolean first = true;
            for (Object e : (java.util.List<?>) v) {
                if (!first) sb.append(", ");
                sb.append(_p(e));
                first = false;
            }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
