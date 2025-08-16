public class Main {
    static String digits = "";
    static long i_3 = 1L;

    static long mod_pow(long base, long exponent, long modulus) {
        long result = 1L;
        long b_1 = Math.floorMod(base, modulus);
        long e_1 = (long)(exponent);
        while ((long)(e_1) > 0L) {
            if (Math.floorMod(e_1, 2) == 1L) {
                result = Math.floorMod(((long)(result) * (long)(b_1)), modulus);
            }
            b_1 = Math.floorMod(((long)(b_1) * (long)(b_1)), modulus);
            e_1 = (long)((long)(e_1) / 2L);
        }
        return result;
    }

    static double pow_float(double base, long exponent) {
        long exp = (long)(exponent);
        double result_2 = (double)(1.0);
        if ((long)(exp) < 0L) {
            exp = (long)(-exp);
        }
        long i_1 = 0L;
        while ((long)(i_1) < (long)(exp)) {
            result_2 = (double)((double)(result_2) * (double)(base));
            i_1 = (long)((long)(i_1) + 1L);
        }
        if ((long)(exponent) < 0L) {
            result_2 = (double)((double)(1.0) / (double)(result_2));
        }
        return result_2;
    }

    static String hex_digit(long n) {
        if ((long)(n) < 10L) {
            return _p(n);
        }
        String[] letters_1 = ((String[])(new String[]{"a", "b", "c", "d", "e", "f"}));
        return letters_1[(int)((long)((long)(n) - 10L))];
    }

    static double floor_float(double x) {
        long i_2 = (long)(((Number)(x)).intValue());
        if ((double)((((Number)(i_2)).doubleValue())) > (double)(x)) {
            i_2 = (long)((long)(i_2) - 1L);
        }
        return ((Number)(i_2)).doubleValue();
    }

    static double subsum(long digit_pos_to_extract, long denominator_addend, long precision) {
        double total = (double)(0.0);
        long sum_index_1 = 0L;
        while ((long)(sum_index_1) < (long)((long)(digit_pos_to_extract) + (long)(precision))) {
            long denominator_1 = (long)((long)(8L * (long)(sum_index_1)) + (long)(denominator_addend));
            if ((long)(sum_index_1) < (long)(digit_pos_to_extract)) {
                long exponent_2 = (long)((long)((long)(digit_pos_to_extract) - 1L) - (long)(sum_index_1));
                long exponential_term_2 = (long)(mod_pow(16L, (long)(exponent_2), (long)(denominator_1)));
                total = (double)((double)(total) + (double)((double)((((Number)(exponential_term_2)).doubleValue())) / (double)((((Number)(denominator_1)).doubleValue()))));
            } else {
                long exponent_3 = (long)((long)((long)(digit_pos_to_extract) - 1L) - (long)(sum_index_1));
                double exponential_term_3 = (double)(pow_float((double)(16.0), (long)(exponent_3)));
                total = (double)((double)(total) + (double)((double)(exponential_term_3) / (double)((((Number)(denominator_1)).doubleValue()))));
            }
            sum_index_1 = (long)((long)(sum_index_1) + 1L);
        }
        return total;
    }

    static String bailey_borwein_plouffe(long digit_position, long precision) {
        if ((long)(digit_position) <= 0L) {
            throw new RuntimeException(String.valueOf("Digit position must be a positive integer"));
        }
        if ((long)(precision) < 0L) {
            throw new RuntimeException(String.valueOf("Precision must be a nonnegative integer"));
        }
        double sum_result_1 = (double)((double)((double)((double)((double)(4.0) * (double)(subsum((long)(digit_position), 1L, (long)(precision)))) - (double)((double)(2.0) * (double)(subsum((long)(digit_position), 4L, (long)(precision))))) - (double)((double)(1.0) * (double)(subsum((long)(digit_position), 5L, (long)(precision))))) - (double)((double)(1.0) * (double)(subsum((long)(digit_position), 6L, (long)(precision)))));
        double fraction_1 = (double)((double)(sum_result_1) - (double)(floor_float((double)(sum_result_1))));
        long digit_1 = (long)(((Number)(((double)(fraction_1) * (double)(16.0)))).intValue());
        String hd_1 = String.valueOf(hex_digit((long)(digit_1)));
        return hd_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            while ((long)(i_3) <= 10L) {
                digits = digits + String.valueOf(bailey_borwein_plouffe((long)(i_3), 1000L));
                i_3 = (long)((long)(i_3) + 1L);
            }
            System.out.println(digits);
            System.out.println(bailey_borwein_plouffe(5L, 10000L));
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
