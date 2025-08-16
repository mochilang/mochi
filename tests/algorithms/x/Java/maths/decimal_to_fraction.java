public class Main {
    static class Fraction {
        long numerator;
        long denominator;
        Fraction(long numerator, long denominator) {
            this.numerator = numerator;
            this.denominator = denominator;
        }
        Fraction() {}
        @Override public String toString() {
            return String.format("{'numerator': %s, 'denominator': %s}", String.valueOf(numerator), String.valueOf(denominator));
        }
    }


    static long pow10(long n) {
        long result = 1L;
        long i_1 = 0L;
        while ((long)(i_1) < (long)(n)) {
            result = (long)((long)(result) * 10L);
            i_1 = (long)((long)(i_1) + 1L);
        }
        return result;
    }

    static long gcd(long a, long b) {
        long x = (long)(a);
        long y_1 = (long)(b);
        if ((long)(x) < 0L) {
            x = (long)(-x);
        }
        if ((long)(y_1) < 0L) {
            y_1 = (long)(-y_1);
        }
        while ((long)(y_1) != 0L) {
            long r_1 = Math.floorMod(x, y_1);
            x = (long)(y_1);
            y_1 = (long)(r_1);
        }
        return x;
    }

    static Fraction parse_decimal(String s) {
        if ((long)(_runeLen(s)) == 0L) {
            throw new RuntimeException(String.valueOf("invalid number"));
        }
        long idx_1 = 0L;
        long sign_1 = 1L;
        String first_1 = _substr(s, (int)(0L), (int)(1L));
        if ((first_1.equals("-"))) {
            sign_1 = (long)(-1);
            idx_1 = 1L;
        } else         if ((first_1.equals("+"))) {
            idx_1 = 1L;
        }
        String int_part_1 = "";
        while ((long)(idx_1) < (long)(_runeLen(s))) {
            String c_1 = _substr(s, (int)((long)(idx_1)), (int)((long)((long)(idx_1) + 1L)));
            if ((c_1.compareTo("0") >= 0) && (c_1.compareTo("9") <= 0)) {
                int_part_1 = int_part_1 + c_1;
                idx_1 = (long)((long)(idx_1) + 1L);
            } else {
                break;
            }
        }
        String frac_part_1 = "";
        if ((long)(idx_1) < (long)(_runeLen(s)) && (_substr(s, (int)((long)(idx_1)), (int)((long)((long)(idx_1) + 1L))).equals("."))) {
            idx_1 = (long)((long)(idx_1) + 1L);
            while ((long)(idx_1) < (long)(_runeLen(s))) {
                String c_3 = _substr(s, (int)((long)(idx_1)), (int)((long)((long)(idx_1) + 1L)));
                if ((c_3.compareTo("0") >= 0) && (c_3.compareTo("9") <= 0)) {
                    frac_part_1 = frac_part_1 + c_3;
                    idx_1 = (long)((long)(idx_1) + 1L);
                } else {
                    break;
                }
            }
        }
        long exp_1 = 0L;
        if ((long)(idx_1) < (long)(_runeLen(s)) && ((_substr(s, (int)((long)(idx_1)), (int)((long)((long)(idx_1) + 1L))).equals("e")) || (_substr(s, (int)((long)(idx_1)), (int)((long)((long)(idx_1) + 1L))).equals("E")))) {
            idx_1 = (long)((long)(idx_1) + 1L);
            long exp_sign_1 = 1L;
            if ((long)(idx_1) < (long)(_runeLen(s)) && (_substr(s, (int)((long)(idx_1)), (int)((long)((long)(idx_1) + 1L))).equals("-"))) {
                exp_sign_1 = (long)(-1);
                idx_1 = (long)((long)(idx_1) + 1L);
            } else             if ((long)(idx_1) < (long)(_runeLen(s)) && (_substr(s, (int)((long)(idx_1)), (int)((long)((long)(idx_1) + 1L))).equals("+"))) {
                idx_1 = (long)((long)(idx_1) + 1L);
            }
            String exp_str_1 = "";
            while ((long)(idx_1) < (long)(_runeLen(s))) {
                String c_5 = _substr(s, (int)((long)(idx_1)), (int)((long)((long)(idx_1) + 1L)));
                if ((c_5.compareTo("0") >= 0) && (c_5.compareTo("9") <= 0)) {
                    exp_str_1 = exp_str_1 + c_5;
                    idx_1 = (long)((long)(idx_1) + 1L);
                } else {
                    throw new RuntimeException(String.valueOf("invalid number"));
                }
            }
            if ((long)(_runeLen(exp_str_1)) == 0L) {
                throw new RuntimeException(String.valueOf("invalid number"));
            }
            exp_1 = (long)((long)(exp_sign_1) * (long)(Integer.parseInt(exp_str_1)));
        }
        if ((long)(idx_1) != (long)(_runeLen(s))) {
            throw new RuntimeException(String.valueOf("invalid number"));
        }
        if ((long)(_runeLen(int_part_1)) == 0L) {
            int_part_1 = "0";
        }
        String num_str_1 = int_part_1 + frac_part_1;
        long numerator_1 = (long)(Integer.parseInt(num_str_1));
        if ((long)(sign_1) == (long)((0L - 1L))) {
            numerator_1 = (long)((0L - (long)(numerator_1)));
        }
        long denominator_1 = (long)(pow10((long)(_runeLen(frac_part_1))));
        if ((long)(exp_1) > 0L) {
            numerator_1 = (long)((long)(numerator_1) * (long)(pow10((long)(exp_1))));
        } else         if ((long)(exp_1) < 0L) {
            denominator_1 = (long)((long)(denominator_1) * (long)(pow10((long)(-exp_1))));
        }
        return new Fraction(numerator_1, denominator_1);
    }

    static Fraction reduce(Fraction fr) {
        long g = (long)(gcd((long)(fr.numerator), (long)(fr.denominator)));
        return new Fraction(Math.floorDiv(((long)(fr.numerator)), ((long)(g))), Math.floorDiv(((long)(fr.denominator)), ((long)(g))));
    }

    static Fraction decimal_to_fraction_str(String s) {
        return reduce(parse_decimal(s));
    }

    static Fraction decimal_to_fraction(double x) {
        return decimal_to_fraction_str(_p(x));
    }

    static void assert_fraction(String name, Fraction fr, long num, long den) {
        if ((long)(fr.numerator) != (long)(num) || (long)(fr.denominator) != (long)(den)) {
            throw new RuntimeException(String.valueOf(name));
        }
    }

    static void test_decimal_to_fraction() {
        assert_fraction("case1", decimal_to_fraction((double)(2.0)), 2L, 1L);
        assert_fraction("case2", decimal_to_fraction((double)(89.0)), 89L, 1L);
        assert_fraction("case3", decimal_to_fraction_str("67"), 67L, 1L);
        assert_fraction("case4", decimal_to_fraction_str("45.0"), 45L, 1L);
        assert_fraction("case5", decimal_to_fraction((double)(1.5)), 3L, 2L);
        assert_fraction("case6", decimal_to_fraction_str("6.25"), 25L, 4L);
        assert_fraction("case7", decimal_to_fraction((double)(0.0)), 0L, 1L);
        assert_fraction("case8", decimal_to_fraction((double)(-2.5)), (long)(-5), 2L);
        assert_fraction("case9", decimal_to_fraction((double)(0.125)), 1L, 8L);
        assert_fraction("case10", decimal_to_fraction((double)(1.00000025e+06)), 4000001L, 4L);
        assert_fraction("case11", decimal_to_fraction((double)(1.3333)), 13333L, 10000L);
        assert_fraction("case12", decimal_to_fraction_str("1.23e2"), 123L, 1L);
        assert_fraction("case13", decimal_to_fraction_str("0.500"), 1L, 2L);
    }

    static void main() {
        test_decimal_to_fraction();
        Fraction fr_1 = decimal_to_fraction((double)(1.5));
        System.out.println(_p(fr_1.numerator) + "/" + _p(fr_1.denominator));
    }
    public static void main(String[] args) {
        main();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int len = _runeLen(s);
        if (i < 0) i = 0;
        if (j > len) j = len;
        if (i > j) i = j;
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
