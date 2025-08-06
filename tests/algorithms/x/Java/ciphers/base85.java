public class Main {
    static String ascii85_chars;

    static int indexOf(String s, String ch) {
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
        int idx = indexOf(ascii85_chars, ch);
        if (idx >= 0) {
            return 33 + idx;
        }
        return 0;
    }

    static String chr(int n) {
        if (n >= 33 && n <= 117) {
            return ascii85_chars.substring(n - 33, n - 32);
        }
        return "?";
    }

    static String to_binary(int n, int bits) {
        String b = "";
        int val = n;
        while (val > 0) {
            b = _p(Math.floorMod(val, 2)) + b;
            val = val / 2;
        }
        while (_runeLen(b) < bits) {
            b = "0" + b;
        }
        if (_runeLen(b) == 0) {
            b = "0";
        }
        return b;
    }

    static int bin_to_int(String bits) {
        int n = 0;
        int i_1 = 0;
        while (i_1 < _runeLen(bits)) {
            if ((bits.substring(i_1, i_1+1).equals("1"))) {
                n = n * 2 + 1;
            } else {
                n = n * 2;
            }
            i_1 = i_1 + 1;
        }
        return n;
    }

    static String reverse(String s) {
        String res = "";
        int i_2 = _runeLen(s) - 1;
        while (i_2 >= 0) {
            res = res + s.substring(i_2, i_2+1);
            i_2 = i_2 - 1;
        }
        return res;
    }

    static String base10_to_85(int d) {
        if (d > 0) {
            return String.valueOf(chr(Math.floorMod(d, 85) + 33)) + String.valueOf(base10_to_85(d / 85));
        }
        return "";
    }

    static int base85_to_10(String digits) {
        int value = 0;
        int i_3 = 0;
        while (i_3 < _runeLen(digits)) {
            value = value * 85 + (ord(digits.substring(i_3, i_3+1)) - 33);
            i_3 = i_3 + 1;
        }
        return value;
    }

    static String ascii85_encode(String data) {
        String binary_data = "";
        for (int _i = 0; _i < data.length(); _i++) {
            var ch = data.substring(_i, _i + 1);
            binary_data = binary_data + String.valueOf(to_binary(ord((String)(ch)), 8));
        }
        int null_values = (32 * ((_runeLen(binary_data) / 32) + 1) - _runeLen(binary_data)) / 8;
        int total_bits = 32 * ((_runeLen(binary_data) / 32) + 1);
        while (_runeLen(binary_data) < total_bits) {
            binary_data = binary_data + "0";
        }
        String result = "";
        int i_4 = 0;
        while (i_4 < _runeLen(binary_data)) {
            String chunk_bits = binary_data.substring(i_4, i_4 + 32);
            int chunk_val = bin_to_int(chunk_bits);
            String encoded = String.valueOf(reverse(String.valueOf(base10_to_85(chunk_val))));
            result = result + encoded;
            i_4 = i_4 + 32;
        }
        if (Math.floorMod(null_values, 4) != 0) {
            result = result.substring(0, _runeLen(result) - null_values);
        }
        return result;
    }

    static String ascii85_decode(String data) {
        int null_values_1 = 5 * ((_runeLen(data) / 5) + 1) - _runeLen(data);
        String binary_data_1 = data;
        int i_5 = 0;
        while (i_5 < null_values_1) {
            binary_data_1 = binary_data_1 + "u";
            i_5 = i_5 + 1;
        }
        String result_1 = "";
        i_5 = 0;
        while (i_5 < _runeLen(binary_data_1)) {
            String chunk = binary_data_1.substring(i_5, i_5 + 5);
            int value_1 = base85_to_10(chunk);
            String bits = String.valueOf(to_binary(value_1, 32));
            int j = 0;
            while (j < 32) {
                String byte_bits = bits.substring(j, j + 8);
                String c = String.valueOf(chr(bin_to_int(byte_bits)));
                result_1 = result_1 + c;
                j = j + 8;
            }
            i_5 = i_5 + 5;
        }
        int trim = null_values_1;
        if (Math.floorMod(null_values_1, 5) == 0) {
            trim = null_values_1 - 1;
        }
        return result_1.substring(0, _runeLen(result_1) - trim);
    }
    public static void main(String[] args) {
        ascii85_chars = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstu";
        System.out.println(ascii85_encode(""));
        System.out.println(ascii85_encode("12345"));
        System.out.println(ascii85_encode("base 85"));
        System.out.println(ascii85_decode(""));
        System.out.println(ascii85_decode("0etOA2#"));
        System.out.println(ascii85_decode("@UX=h+?24"));
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
