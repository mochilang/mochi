public class Main {

    static void panic(String msg) {
        System.out.println(msg);
    }

    static String trim_spaces(String s) {
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

    static int hex_digit_value(String ch) {
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
        if ((ch.equals("8"))) {
            return 8;
        }
        if ((ch.equals("9"))) {
            return 9;
        }
        if ((ch.equals("a")) || (ch.equals("A"))) {
            return 10;
        }
        if ((ch.equals("b")) || (ch.equals("B"))) {
            return 11;
        }
        if ((ch.equals("c")) || (ch.equals("C"))) {
            return 12;
        }
        if ((ch.equals("d")) || (ch.equals("D"))) {
            return 13;
        }
        if ((ch.equals("e")) || (ch.equals("E"))) {
            return 14;
        }
        if ((ch.equals("f")) || (ch.equals("F"))) {
            return 15;
        }
        throw new RuntimeException(String.valueOf("Invalid value was passed to the function"));
    }

    static int hex_to_bin(String hex_num) {
        String trimmed = String.valueOf(trim_spaces(hex_num));
        if (_runeLen(trimmed) == 0) {
            throw new RuntimeException(String.valueOf("No value was passed to the function"));
        }
        String s = trimmed;
        boolean is_negative = false;
        if ((_substr(s, 0, 1).equals("-"))) {
            is_negative = true;
            s = _substr(s, 1, _runeLen(s));
        }
        int int_num = 0;
        int i = 0;
        while (i < _runeLen(s)) {
            String ch = _substr(s, i, i + 1);
            int val = hex_digit_value(ch);
            int_num = int_num * 16 + val;
            i = i + 1;
        }
        String bin_str = "";
        int n = int_num;
        if (n == 0) {
            bin_str = "0";
        }
        while (n > 0) {
            bin_str = _p(Math.floorMod(n, 2)) + bin_str;
            n = n / 2;
        }
        int result = Integer.parseInt(bin_str);
        if (is_negative) {
            result = -result;
        }
        return result;
    }
    public static void main(String[] args) {
        System.out.println(_p(hex_to_bin("AC")));
        System.out.println(_p(hex_to_bin("9A4")));
        System.out.println(_p(hex_to_bin("   12f   ")));
        System.out.println(_p(hex_to_bin("FfFf")));
        System.out.println(_p(hex_to_bin("-fFfF")));
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
