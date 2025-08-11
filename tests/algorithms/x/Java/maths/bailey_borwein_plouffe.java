public class Main {
    static String digits = null;
    static long i_3 = 0;

    static long mod_pow(long base, long exponent, long modulus) {
        long result = 1;
        long b_1 = Math.floorMod(base, modulus);
        long e_1 = exponent;
        while (e_1 > 0) {
            if (Math.floorMod(e_1, 2) == 1) {
                result = Math.floorMod((result * b_1), modulus);
            }
            b_1 = Math.floorMod((b_1 * b_1), modulus);
            e_1 = Math.floorDiv(e_1, 2);
        }
        return result;
    }

    static double pow_float(double base, long exponent) {
        long exp = exponent;
        double result_2 = 1.0;
        if (exp < 0) {
            exp = -exp;
        }
        long i_1 = 0;
        while (i_1 < exp) {
            result_2 = result_2 * base;
            i_1 = i_1 + 1;
        }
        if (exponent < 0) {
            result_2 = 1.0 / result_2;
        }
        return result_2;
    }

    static String hex_digit(long n) {
        if (n < 10) {
            return _p(n);
        }
        String[] letters_1 = ((String[])(new String[]{"a", "b", "c", "d", "e", "f"}));
        return letters_1[(int)(n - 10)];
    }

    static double floor_float(double x) {
        long i_2 = ((Number)(x)).intValue();
        if ((((Number)(i_2)).doubleValue()) > x) {
            i_2 = i_2 - 1;
        }
        return ((Number)(i_2)).doubleValue();
    }

    static double subsum(long digit_pos_to_extract, long denominator_addend, long precision) {
        double total = 0.0;
        long sum_index_1 = 0;
        while (sum_index_1 < digit_pos_to_extract + precision) {
            long denominator_1 = 8 * sum_index_1 + denominator_addend;
            if (sum_index_1 < digit_pos_to_extract) {
                long exponent_2 = digit_pos_to_extract - 1 - sum_index_1;
                long exponential_term_2 = mod_pow(16, exponent_2, denominator_1);
                total = total + (((Number)(exponential_term_2)).doubleValue()) / (((Number)(denominator_1)).doubleValue());
            } else {
                long exponent_3 = digit_pos_to_extract - 1 - sum_index_1;
                double exponential_term_3 = pow_float(16.0, exponent_3);
                total = total + exponential_term_3 / (((Number)(denominator_1)).doubleValue());
            }
            sum_index_1 = sum_index_1 + 1;
        }
        return total;
    }

    static String bailey_borwein_plouffe(long digit_position, long precision) {
        if (digit_position <= 0) {
            throw new RuntimeException(String.valueOf("Digit position must be a positive integer"));
        }
        if (precision < 0) {
            throw new RuntimeException(String.valueOf("Precision must be a nonnegative integer"));
        }
        double sum_result_1 = 4.0 * subsum(digit_position, 1, precision) - 2.0 * subsum(digit_position, 4, precision) - 1.0 * subsum(digit_position, 5, precision) - 1.0 * subsum(digit_position, 6, precision);
        double fraction_1 = sum_result_1 - floor_float(sum_result_1);
        long digit_1 = ((Number)((fraction_1 * 16.0))).intValue();
        String hd_1 = String.valueOf(hex_digit(digit_1));
        return hd_1;
    }
    public static void main(String[] args) {
        digits = "";
        i_3 = 1;
        while (i_3 <= 10) {
            digits = digits + String.valueOf(bailey_borwein_plouffe(i_3, 1000));
            i_3 = i_3 + 1;
        }
        System.out.println(digits);
        System.out.println(bailey_borwein_plouffe(5, 10000));
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
