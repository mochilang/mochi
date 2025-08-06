public class Main {
    static class Fraction {
        int numerator;
        int denominator;
        Fraction(int numerator, int denominator) {
            this.numerator = numerator;
            this.denominator = denominator;
        }
        Fraction() {}
        @Override public String toString() {
            return String.format("{'numerator': %s, 'denominator': %s}", String.valueOf(numerator), String.valueOf(denominator));
        }
    }


    static int pow10(int n) {
        int result = 1;
        int i = 0;
        while (i < n) {
            result = result * 10;
            i = i + 1;
        }
        return result;
    }

    static int gcd(int a, int b) {
        int x = a;
        int y = b;
        if (x < 0) {
            x = -x;
        }
        if (y < 0) {
            y = -y;
        }
        while (y != 0) {
            int r = Math.floorMod(x, y);
            x = y;
            y = r;
        }
        return x;
    }

    static Fraction parse_decimal(String s) {
        if (_runeLen(s) == 0) {
            throw new RuntimeException(String.valueOf("invalid number"));
        }
        int idx = 0;
        int sign = 1;
        String first = _substr(s, 0, 1);
        if ((first.equals("-"))) {
            sign = -1;
            idx = 1;
        } else         if ((first.equals("+"))) {
            idx = 1;
        }
        String int_part = "";
        while (idx < _runeLen(s)) {
            String c = _substr(s, idx, idx + 1);
            if ((c.compareTo("0") >= 0) && (c.compareTo("9") <= 0)) {
                int_part = int_part + c;
                idx = idx + 1;
            } else {
                break;
            }
        }
        String frac_part = "";
        if (idx < _runeLen(s) && (_substr(s, idx, idx + 1).equals("."))) {
            idx = idx + 1;
            while (idx < _runeLen(s)) {
                String c_1 = _substr(s, idx, idx + 1);
                if ((c_1.compareTo("0") >= 0) && (c_1.compareTo("9") <= 0)) {
                    frac_part = frac_part + c_1;
                    idx = idx + 1;
                } else {
                    break;
                }
            }
        }
        int exp = 0;
        if (idx < _runeLen(s) && ((_substr(s, idx, idx + 1).equals("e")) || (_substr(s, idx, idx + 1).equals("E")))) {
            idx = idx + 1;
            int exp_sign = 1;
            if (idx < _runeLen(s) && (_substr(s, idx, idx + 1).equals("-"))) {
                exp_sign = -1;
                idx = idx + 1;
            } else             if (idx < _runeLen(s) && (_substr(s, idx, idx + 1).equals("+"))) {
                idx = idx + 1;
            }
            String exp_str = "";
            while (idx < _runeLen(s)) {
                String c_2 = _substr(s, idx, idx + 1);
                if ((c_2.compareTo("0") >= 0) && (c_2.compareTo("9") <= 0)) {
                    exp_str = exp_str + c_2;
                    idx = idx + 1;
                } else {
                    throw new RuntimeException(String.valueOf("invalid number"));
                }
            }
            if (_runeLen(exp_str) == 0) {
                throw new RuntimeException(String.valueOf("invalid number"));
            }
            exp = exp_sign * Integer.parseInt(exp_str);
        }
        if (idx != _runeLen(s)) {
            throw new RuntimeException(String.valueOf("invalid number"));
        }
        if (_runeLen(int_part) == 0) {
            int_part = "0";
        }
        String num_str = int_part + frac_part;
        int numerator = Integer.parseInt(num_str);
        if (sign == (0 - 1)) {
            numerator = (0 - numerator);
        }
        int denominator = pow10(_runeLen(frac_part));
        if (exp > 0) {
            numerator = numerator * pow10(exp);
        } else         if (exp < 0) {
            denominator = denominator * pow10(-exp);
        }
        return new Fraction(numerator, denominator);
    }

    static Fraction reduce(Fraction fr) {
        int g = gcd(fr.numerator, fr.denominator);
        return new Fraction(fr.numerator / g, fr.denominator / g);
    }

    static Fraction decimal_to_fraction_str(String s) {
        return reduce(parse_decimal(s));
    }

    static Fraction decimal_to_fraction(double x) {
        return decimal_to_fraction_str(_p(x));
    }

    static void assert_fraction(String name, Fraction fr, int num, int den) {
        if (fr.numerator != num || fr.denominator != den) {
            throw new RuntimeException(String.valueOf(name));
        }
    }

    static void test_decimal_to_fraction() {
        assert_fraction("case1", decimal_to_fraction(2.0), 2, 1);
        assert_fraction("case2", decimal_to_fraction(89.0), 89, 1);
        assert_fraction("case3", decimal_to_fraction_str("67"), 67, 1);
        assert_fraction("case4", decimal_to_fraction_str("45.0"), 45, 1);
        assert_fraction("case5", decimal_to_fraction(1.5), 3, 2);
        assert_fraction("case6", decimal_to_fraction_str("6.25"), 25, 4);
        assert_fraction("case7", decimal_to_fraction(0.0), 0, 1);
        assert_fraction("case8", decimal_to_fraction(-2.5), -5, 2);
        assert_fraction("case9", decimal_to_fraction(0.125), 1, 8);
        assert_fraction("case10", decimal_to_fraction(1.00000025e+06), 4000001, 4);
        assert_fraction("case11", decimal_to_fraction(1.3333), 13333, 10000);
        assert_fraction("case12", decimal_to_fraction_str("1.23e2"), 123, 1);
        assert_fraction("case13", decimal_to_fraction_str("0.500"), 1, 2);
    }

    static void main() {
        test_decimal_to_fraction();
        Fraction fr = decimal_to_fraction(1.5);
        System.out.println(_p(fr.numerator) + "/" + _p(fr.denominator));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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
