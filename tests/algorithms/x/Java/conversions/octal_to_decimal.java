public class Main {

    static void panic(String msg) {
        System.out.println(msg);
    }

    static String trim_spaces(String s) {
        int start = 0;
        int end = _runeLen(s) - 1;
        while (start <= end && (_substr(s, start, start + 1).equals(" "))) {
            start = start + 1;
        }
        while (end >= start && (_substr(s, end, end + 1).equals(" "))) {
            end = end - 1;
        }
        if (start > end) {
            return "";
        }
        return _substr(s, start, end + 1);
    }

    static int char_to_digit(String ch) {
        if ((ch.equals("0"))) {
            return 0;
        }
        if ((ch.equals("1"))) {
            return 1;
        }
        if ((ch.equals("2"))) {
            return 2;
        }
        if ((ch.equals("3"))) {
            return 3;
        }
        if ((ch.equals("4"))) {
            return 4;
        }
        if ((ch.equals("5"))) {
            return 5;
        }
        if ((ch.equals("6"))) {
            return 6;
        }
        if ((ch.equals("7"))) {
            return 7;
        }
        throw new RuntimeException(String.valueOf("Non-octal value was passed to the function"));
        return 0;
    }

    static int oct_to_decimal(String oct_string) {
        String s = String.valueOf(trim_spaces(oct_string));
        if (_runeLen(s) == 0) {
            throw new RuntimeException(String.valueOf("Empty string was passed to the function"));
            return 0;
        }
        boolean is_negative = false;
        if ((_substr(s, 0, 1).equals("-"))) {
            is_negative = true;
            s = _substr(s, 1, _runeLen(s));
        }
        if (_runeLen(s) == 0) {
            throw new RuntimeException(String.valueOf("Non-octal value was passed to the function"));
            return 0;
        }
        int decimal_number = 0;
        int i = 0;
        while (i < _runeLen(s)) {
            String ch = _substr(s, i, i + 1);
            int digit = char_to_digit(ch);
            decimal_number = 8 * decimal_number + digit;
            i = i + 1;
        }
        if (is_negative) {
            decimal_number = -decimal_number;
        }
        return decimal_number;
    }

    static void main() {
        System.out.println(_p(oct_to_decimal("1")));
        System.out.println(_p(oct_to_decimal("-1")));
        System.out.println(_p(oct_to_decimal("12")));
        System.out.println(_p(oct_to_decimal(" 12   ")));
        System.out.println(_p(oct_to_decimal("-45")));
        System.out.println(_p(oct_to_decimal("0")));
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
