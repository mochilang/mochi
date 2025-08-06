public class Main {

    static String trim(String s) {
        int start = 0;
        while (start < _runeLen(s)) {
            String ch = s.substring(start, start + 1);
            if (!(ch.equals(" ")) && !(ch.equals("\n")) && !(ch.equals("\t")) && !(ch.equals("\r"))) {
                break;
            }
            start = start + 1;
        }
        int end = _runeLen(s);
        while (end > start) {
            String ch_1 = s.substring(end - 1, end);
            if (!(ch_1.equals(" ")) && !(ch_1.equals("\n")) && !(ch_1.equals("\t")) && !(ch_1.equals("\r"))) {
                break;
            }
            end = end - 1;
        }
        return _substr(s, start, end);
    }

    static int bin_to_decimal(String bin_string) {
        String trimmed = String.valueOf(trim(bin_string));
        if ((trimmed.equals(""))) {
            throw new RuntimeException(String.valueOf("Empty string was passed to the function"));
        }
        boolean is_negative = false;
        String s = trimmed;
        if ((s.substring(0, 1).equals("-"))) {
            is_negative = true;
            s = _substr(s, 1, _runeLen(s));
        }
        int i = 0;
        while (i < _runeLen(s)) {
            String c = s.substring(i, i + 1);
            if (!(c.equals("0")) && !(c.equals("1"))) {
                throw new RuntimeException(String.valueOf("Non-binary value was passed to the function"));
            }
            i = i + 1;
        }
        int decimal_number = 0;
        i = 0;
        while (i < _runeLen(s)) {
            String c_1 = s.substring(i, i + 1);
            int digit = Integer.parseInt(c_1);
            decimal_number = 2 * decimal_number + digit;
            i = i + 1;
        }
        if (is_negative) {
            return -decimal_number;
        }
        return decimal_number;
    }
    public static void main(String[] args) {
        System.out.println(_p(bin_to_decimal("101")));
        System.out.println(_p(bin_to_decimal(" 1010   ")));
        System.out.println(_p(bin_to_decimal("-11101")));
        System.out.println(_p(bin_to_decimal("0")));
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
