public class Main {
    static String B32_CHARSET;

    static int indexOfChar(String s, String ch) {
        int i = 0;
        while (i < _runeLen(s)) {
            if ((s.substring(i, i+1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static int ord(String ch) {
        String upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String lower = "abcdefghijklmnopqrstuvwxyz";
        String digits = "0123456789";
        int idx = indexOfChar(upper, ch);
        if (idx >= 0) {
            return 65 + idx;
        }
        idx = indexOfChar(lower, ch);
        if (idx >= 0) {
            return 97 + idx;
        }
        idx = indexOfChar(digits, ch);
        if (idx >= 0) {
            return 48 + idx;
        }
        if ((ch.equals(" "))) {
            return 32;
        }
        if ((ch.equals("!"))) {
            return 33;
        }
        return 0;
    }

    static String chr(int code) {
        String upper_1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String lower_1 = "abcdefghijklmnopqrstuvwxyz";
        String digits_1 = "0123456789";
        if (code == 32) {
            return " ";
        }
        if (code == 33) {
            return "!";
        }
        int idx_1 = code - 65;
        if (idx_1 >= 0 && idx_1 < _runeLen(upper_1)) {
            return upper_1.substring(idx_1, idx_1+1);
        }
        idx_1 = code - 97;
        if (idx_1 >= 0 && idx_1 < _runeLen(lower_1)) {
            return lower_1.substring(idx_1, idx_1+1);
        }
        idx_1 = code - 48;
        if (idx_1 >= 0 && idx_1 < _runeLen(digits_1)) {
            return digits_1.substring(idx_1, idx_1+1);
        }
        return "";
    }

    static String repeat(String s, int n) {
        String out = "";
        int i_1 = 0;
        while (i_1 < n) {
            out = out + s;
            i_1 = i_1 + 1;
        }
        return out;
    }

    static String to_binary(int n, int bits) {
        int v = n;
        String out_1 = "";
        int i_2 = 0;
        while (i_2 < bits) {
            out_1 = _p(Math.floorMod(v, 2)) + out_1;
            v = v / 2;
            i_2 = i_2 + 1;
        }
        return out_1;
    }

    static int binary_to_int(String bits) {
        int n = 0;
        int i_3 = 0;
        while (i_3 < _runeLen(bits)) {
            n = n * 2;
            if ((bits.substring(i_3, i_3+1).equals("1"))) {
                n = n + 1;
            }
            i_3 = i_3 + 1;
        }
        return n;
    }

    static String base32_encode(String data) {
        String binary_data = "";
        int i_4 = 0;
        while (i_4 < _runeLen(data)) {
            binary_data = binary_data + String.valueOf(to_binary(ord(data.substring(i_4, i_4+1)), 8));
            i_4 = i_4 + 1;
        }
        int remainder = Math.floorMod(_runeLen(binary_data), 5);
        if (remainder != 0) {
            binary_data = binary_data + (String)(_repeat("0", 5 - remainder));
        }
        String b32_result = "";
        int j = 0;
        while (j < _runeLen(binary_data)) {
            String chunk = binary_data.substring(j, j + 5);
            int index = binary_to_int(chunk);
            b32_result = b32_result + B32_CHARSET.substring(index, index+1);
            j = j + 5;
        }
        int rem = Math.floorMod(_runeLen(b32_result), 8);
        if (rem != 0) {
            b32_result = b32_result + (String)(_repeat("=", 8 - rem));
        }
        return b32_result;
    }

    static String base32_decode(String data) {
        String clean = "";
        int i_5 = 0;
        while (i_5 < _runeLen(data)) {
            String ch = data.substring(i_5, i_5+1);
            if (!(ch.equals("="))) {
                clean = clean + ch;
            }
            i_5 = i_5 + 1;
        }
        String binary_chunks = "";
        i_5 = 0;
        while (i_5 < _runeLen(clean)) {
            int idx_2 = indexOfChar(B32_CHARSET, clean.substring(i_5, i_5+1));
            binary_chunks = binary_chunks + String.valueOf(to_binary(idx_2, 5));
            i_5 = i_5 + 1;
        }
        String result = "";
        int j_1 = 0;
        while (j_1 + 8 <= _runeLen(binary_chunks)) {
            String byte_bits = binary_chunks.substring(j_1, j_1 + 8);
            int code = binary_to_int(byte_bits);
            result = result + String.valueOf(chr(code));
            j_1 = j_1 + 8;
        }
        return result;
    }
    public static void main(String[] args) {
        B32_CHARSET = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567";
        System.out.println(base32_encode("Hello World!"));
        System.out.println(base32_encode("123456"));
        System.out.println(base32_encode("some long complex string"));
        System.out.println(base32_decode("JBSWY3DPEBLW64TMMQQQ===="));
        System.out.println(base32_decode("GEZDGNBVGY======"));
        System.out.println(base32_decode("ONXW2ZJANRXW4ZZAMNXW24DMMV4CA43UOJUW4ZY="));
    }

    static String _repeat(String s, int n) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < n; i++) sb.append(s);
        return sb.toString();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
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
