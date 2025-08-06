public class Main {

    static String strip(String s) {
        int start = 0;
        int end = _runeLen(s);
        while (start < end && (_substr(s, start, start + 1).equals(" "))) {
            start = start + 1;
        }
        while (end > start && (_substr(s, end - 1, end).equals(" "))) {
            end = end - 1;
        }
        return _substr(s, start, end);
    }

    static int hex_digit_value(String c) {
        if ((c.equals("0"))) {
            return 0;
        }
        if ((c.equals("1"))) {
            return 1;
        }
        if ((c.equals("2"))) {
            return 2;
        }
        if ((c.equals("3"))) {
            return 3;
        }
        if ((c.equals("4"))) {
            return 4;
        }
        if ((c.equals("5"))) {
            return 5;
        }
        if ((c.equals("6"))) {
            return 6;
        }
        if ((c.equals("7"))) {
            return 7;
        }
        if ((c.equals("8"))) {
            return 8;
        }
        if ((c.equals("9"))) {
            return 9;
        }
        if ((c.equals("a")) || (c.equals("A"))) {
            return 10;
        }
        if ((c.equals("b")) || (c.equals("B"))) {
            return 11;
        }
        if ((c.equals("c")) || (c.equals("C"))) {
            return 12;
        }
        if ((c.equals("d")) || (c.equals("D"))) {
            return 13;
        }
        if ((c.equals("e")) || (c.equals("E"))) {
            return 14;
        }
        if ((c.equals("f")) || (c.equals("F"))) {
            return 15;
        }
        System.out.println("Non-hexadecimal value was passed to the function");
        return 0;
    }

    static int hex_to_decimal(String hex_string) {
        String s = String.valueOf(strip(hex_string));
        if (_runeLen(s) == 0) {
            System.out.println("Empty string was passed to the function");
            return 0;
        }
        boolean is_negative = false;
        if ((_substr(s, 0, 1).equals("-"))) {
            is_negative = true;
            s = _substr(s, 1, _runeLen(s));
        }
        int decimal_number = 0;
        int i = 0;
        while (i < _runeLen(s)) {
            String c = _substr(s, i, i + 1);
            int value = hex_digit_value(c);
            decimal_number = 16 * decimal_number + value;
            i = i + 1;
        }
        if (is_negative) {
            return -decimal_number;
        }
        return decimal_number;
    }

    static void main() {
        System.out.println(_p(hex_to_decimal("a")));
        System.out.println(_p(hex_to_decimal("12f")));
        System.out.println(_p(hex_to_decimal("   12f   ")));
        System.out.println(_p(hex_to_decimal("FfFf")));
        System.out.println(_p(hex_to_decimal("-Ff")));
    }
    public static void main(String[] args) {
        main();
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
