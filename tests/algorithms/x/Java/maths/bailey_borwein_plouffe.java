public class Main {
    static String digits = null;
    static int i_2 = 0;

    static int mod_pow(int base, int exponent, int modulus) {
        int result = 1;
        int b = Math.floorMod(base, modulus);
        int e = exponent;
        while (e > 0) {
            if (Math.floorMod(e, 2) == 1) {
                result = Math.floorMod((result * b), modulus);
            }
            b = Math.floorMod((b * b), modulus);
            e = e / 2;
        }
        return result;
    }

    static double pow_float(double base, int exponent) {
        int exp = exponent;
        double result_1 = 1.0;
        if (exp < 0) {
            exp = -exp;
        }
        int i = 0;
        while (i < exp) {
            result_1 = result_1 * base;
            i = i + 1;
        }
        if (exponent < 0) {
            result_1 = 1.0 / result_1;
        }
        return result_1;
    }

    static String hex_digit(int n) {
        if (n < 10) {
            return _p(n);
        }
        String[] letters = ((String[])(new String[]{"a", "b", "c", "d", "e", "f"}));
        return letters[n - 10];
    }

    static double floor_float(double x) {
        int i_1 = ((Number)(x)).intValue();
        if ((((Number)(i_1)).doubleValue()) > x) {
            i_1 = i_1 - 1;
        }
        return ((Number)(i_1)).doubleValue();
    }

    static double subsum(int digit_pos_to_extract, int denominator_addend, int precision) {
        double total = 0.0;
        int sum_index = 0;
        while (sum_index < digit_pos_to_extract + precision) {
            int denominator = 8 * sum_index + denominator_addend;
            if (sum_index < digit_pos_to_extract) {
                int exponent = digit_pos_to_extract - 1 - sum_index;
                int exponential_term = mod_pow(16, exponent, denominator);
                total = total + (((Number)(exponential_term)).doubleValue()) / (((Number)(denominator)).doubleValue());
            } else {
                int exponent_1 = digit_pos_to_extract - 1 - sum_index;
                double exponential_term_1 = pow_float(16.0, exponent_1);
                total = total + exponential_term_1 / (((Number)(denominator)).doubleValue());
            }
            sum_index = sum_index + 1;
        }
        return total;
    }

    static String bailey_borwein_plouffe(int digit_position, int precision) {
        if (digit_position <= 0) {
            throw new RuntimeException(String.valueOf("Digit position must be a positive integer"));
        }
        if (precision < 0) {
            throw new RuntimeException(String.valueOf("Precision must be a nonnegative integer"));
        }
        double sum_result = 4.0 * subsum(digit_position, 1, precision) - 2.0 * subsum(digit_position, 4, precision) - 1.0 * subsum(digit_position, 5, precision) - 1.0 * subsum(digit_position, 6, precision);
        double fraction = sum_result - floor_float(sum_result);
        int digit = ((Number)((fraction * 16.0))).intValue();
        String hd = String.valueOf(hex_digit(digit));
        return hd;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            digits = "";
            i_2 = 1;
            while (i_2 <= 10) {
                digits = digits + String.valueOf(bailey_borwein_plouffe(i_2, 1000));
                i_2 = i_2 + 1;
            }
            System.out.println(digits);
            System.out.println(bailey_borwein_plouffe(5, 10000));
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
