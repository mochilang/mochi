public class Main {
    static String B64_CHARSET;

    static String to_binary(int n) {
        if (n == 0) {
            return "0";
        }
        int num = n;
        String res = "";
        while (num > 0) {
            int bit = Math.floorMod(num, 2);
            res = _p(bit) + res;
            num = num / 2;
        }
        return res;
    }

    static String zfill(String s, int width) {
        String res_1 = s;
        int pad = width - _runeLen(s);
        while (pad > 0) {
            res_1 = "0" + res_1;
            pad = pad - 1;
        }
        return res_1;
    }

    static int from_binary(String s) {
        int i = 0;
        int result = 0;
        while (i < _runeLen(s)) {
            result = result * 2;
            if ((_substr(s, i, i + 1).equals("1"))) {
                result = result + 1;
            }
            i = i + 1;
        }
        return result;
    }

    static String repeat(String ch, int times) {
        String res_2 = "";
        int i_1 = 0;
        while (i_1 < times) {
            res_2 = res_2 + ch;
            i_1 = i_1 + 1;
        }
        return res_2;
    }

    static int char_index(String s, String c) {
        int i_2 = 0;
        while (i_2 < _runeLen(s)) {
            if ((_substr(s, i_2, i_2 + 1).equals(c))) {
                return i_2;
            }
            i_2 = i_2 + 1;
        }
        return -1;
    }

    static String base64_encode(int[] data) {
        String bits = "";
        int i_3 = 0;
        while (i_3 < data.length) {
            bits = bits + String.valueOf(zfill(String.valueOf(to_binary(data[i_3])), 8));
            i_3 = i_3 + 1;
        }
        int pad_bits = 0;
        if (Math.floorMod(_runeLen(bits), 6) != 0) {
            pad_bits = 6 - Math.floorMod(_runeLen(bits), 6);
            bits = bits + (String)(_repeat("0", pad_bits));
        }
        int j = 0;
        String encoded = "";
        while (j < _runeLen(bits)) {
            String chunk = _substr(bits, j, j + 6);
            int idx = from_binary(chunk);
            encoded = encoded + _substr(B64_CHARSET, idx, idx + 1);
            j = j + 6;
        }
        int pad_1 = pad_bits / 2;
        while (pad_1 > 0) {
            encoded = encoded + "=";
            pad_1 = pad_1 - 1;
        }
        return encoded;
    }

    static int[] base64_decode(String s) {
        int padding = 0;
        int end = _runeLen(s);
        while (end > 0 && (_substr(s, end - 1, end).equals("="))) {
            padding = padding + 1;
            end = end - 1;
        }
        String bits_1 = "";
        int k = 0;
        while (k < end) {
            String c = _substr(s, k, k + 1);
            int idx_1 = char_index(B64_CHARSET, c);
            bits_1 = bits_1 + String.valueOf(zfill(String.valueOf(to_binary(idx_1)), 6));
            k = k + 1;
        }
        if (padding > 0) {
            bits_1 = _substr(bits_1, 0, _runeLen(bits_1) - padding * 2);
        }
        int[] bytes = ((int[])(new int[]{}));
        int m = 0;
        while (m < _runeLen(bits_1)) {
            int byte_ = from_binary(_substr(bits_1, m, m + 8));
            bytes = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(bytes), java.util.stream.IntStream.of(byte_)).toArray()));
            m = m + 8;
        }
        return bytes;
    }

    static void main() {
        int[] data = ((int[])(new int[]{77, 111, 99, 104, 105}));
        String encoded_1 = String.valueOf(base64_encode(((int[])(data))));
        System.out.println(encoded_1);
        json(base64_decode(encoded_1));
    }
    public static void main(String[] args) {
        B64_CHARSET = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
        main();
    }

    static String _repeat(String s, int n) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < n; i++) sb.append(s);
        return sb.toString();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }

    static void json(Object v) {
        System.out.println(_json(v));
    }

    static String _json(Object v) {
        if (v == null) return "null";
        if (v instanceof String) {
            String s = (String)v;
            s = s.replace("\\", "\\\\").replace("\"", "\\\"");
            return "\"" + s + "\"";
        }
        if (v instanceof Number || v instanceof Boolean) {
            return String.valueOf(v);
        }
        if (v instanceof int[]) {
            int[] a = (int[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(a[i]); }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof double[]) {
            double[] a = (double[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(a[i]); }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof boolean[]) {
            boolean[] a = (boolean[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(a[i]); }
            sb.append("]");
            return sb.toString();
        }
        if (v.getClass().isArray()) {
            Object[] a = (Object[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(_json(a[i])); }
            sb.append("]");
            return sb.toString();
        }
        String s = String.valueOf(v);
        s = s.replace("\\", "\\\\").replace("\"", "\\\"");
        return "\"" + s + "\"";
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
