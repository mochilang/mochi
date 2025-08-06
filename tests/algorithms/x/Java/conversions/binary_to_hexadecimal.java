public class Main {

    static String strip_spaces(String s) {
        int start = 0;
        int end = _runeLen(s) - 1;
        while (start < _runeLen(s) && (s.substring(start, start+1).equals(" "))) {
            start = start + 1;
        }
        while (end >= start && (s.substring(end, end+1).equals(" "))) {
            end = end - 1;
        }
        String res = "";
        int i = start;
        while (i <= end) {
            res = res + s.substring(i, i+1);
            i = i + 1;
        }
        return res;
    }

    static String repeat_char(String ch, int count) {
        String res_1 = "";
        int i_1 = 0;
        while (i_1 < count) {
            res_1 = res_1 + ch;
            i_1 = i_1 + 1;
        }
        return res_1;
    }

    static String slice(String s, int start, int end) {
        String res_2 = "";
        int i_2 = start;
        while (i_2 < end) {
            res_2 = res_2 + s.substring(i_2, i_2+1);
            i_2 = i_2 + 1;
        }
        return res_2;
    }

    static int bits_to_int(String bits) {
        int value = 0;
        int i_3 = 0;
        while (i_3 < _runeLen(bits)) {
            value = value * 2;
            if ((bits.substring(i_3, i_3+1).equals("1"))) {
                value = value + 1;
            }
            i_3 = i_3 + 1;
        }
        return value;
    }

    static String bin_to_hexadecimal(String binary_str) {
        String s = String.valueOf(strip_spaces(binary_str));
        if (_runeLen(s) == 0) {
            throw new RuntimeException(String.valueOf("Empty string was passed to the function"));
        }
        boolean is_negative = false;
        if ((s.substring(0, 0+1).equals("-"))) {
            is_negative = true;
            s = s.substring(1, _runeLen(s));
        }
        int i_4 = 0;
        while (i_4 < _runeLen(s)) {
            String c = s.substring(i_4, i_4+1);
            if (!(c.equals("0")) && !(c.equals("1"))) {
                throw new RuntimeException(String.valueOf("Non-binary value was passed to the function"));
            }
            i_4 = i_4 + 1;
        }
        int groups = _runeLen(s) / 4 + 1;
        int pad_len = groups * 4 - _runeLen(s);
        s = String.valueOf(repeat_char("0", pad_len)) + s;
        String digits = "0123456789abcdef";
        String res_3 = "0x";
        int j = 0;
        while (j < _runeLen(s)) {
            String chunk = s.substring(j, j + 4);
            int val = bits_to_int(chunk);
            res_3 = res_3 + digits.substring(val, val+1);
            j = j + 4;
        }
        if (is_negative) {
            return "-" + res_3;
        }
        return res_3;
    }
    public static void main(String[] args) {
        System.out.println(bin_to_hexadecimal("101011111"));
        System.out.println(bin_to_hexadecimal(" 1010   "));
        System.out.println(bin_to_hexadecimal("-11101"));
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
