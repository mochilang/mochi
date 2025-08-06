public class Main {

    static String repeat_char(String ch, int count) {
        String res = "";
        int i = 0;
        while (i < count) {
            res = res + ch;
            i = i + 1;
        }
        return res;
    }

    static int abs_int(int n) {
        if (n < 0) {
            return -n;
        }
        return n;
    }

    static int pow2(int exp) {
        int res_1 = 1;
        int i_1 = 0;
        while (i_1 < exp) {
            res_1 = res_1 * 2;
            i_1 = i_1 + 1;
        }
        return res_1;
    }

    static String to_binary_no_prefix(int n) {
        int v = n;
        if (v < 0) {
            v = -v;
        }
        if (v == 0) {
            return "0";
        }
        String res_2 = "";
        while (v > 0) {
            res_2 = _p(Math.floorMod(v, 2)) + res_2;
            v = v / 2;
        }
        return res_2;
    }

    static String logical_left_shift(int number, int shift_amount) {
        if (number < 0 || shift_amount < 0) {
            throw new RuntimeException(String.valueOf("both inputs must be positive integers"));
        }
        String binary_number = "0b" + String.valueOf(to_binary_no_prefix(number));
        return binary_number + String.valueOf(repeat_char("0", shift_amount));
    }

    static String logical_right_shift(int number, int shift_amount) {
        if (number < 0 || shift_amount < 0) {
            throw new RuntimeException(String.valueOf("both inputs must be positive integers"));
        }
        String binary_number_1 = String.valueOf(to_binary_no_prefix(number));
        if (shift_amount >= _runeLen(binary_number_1)) {
            return "0b0";
        }
        String shifted = _substr(binary_number_1, 0, _runeLen(binary_number_1) - shift_amount);
        return "0b" + shifted;
    }

    static String arithmetic_right_shift(int number, int shift_amount) {
        String binary_number_3;
        if (number >= 0) {
            binary_number_3 = "0" + String.valueOf(to_binary_no_prefix(number));
        } else {
            int length = String.valueOf(to_binary_no_prefix(-number)).length();
            int intermediate = abs_int(number) - pow2(length);
            String bin_repr = String.valueOf(to_binary_no_prefix(intermediate));
            binary_number_3 = "1" + String.valueOf(repeat_char("0", length - _runeLen(bin_repr))) + bin_repr;
        }
        if (shift_amount >= _runeLen(binary_number_3)) {
            String sign = _substr(binary_number_3, 0, 1);
            return "0b" + String.valueOf(repeat_char(sign, _runeLen(binary_number_3)));
        }
        String sign_1 = _substr(binary_number_3, 0, 1);
        String shifted_1 = _substr(binary_number_3, 0, _runeLen(binary_number_3) - shift_amount);
        return "0b" + String.valueOf(repeat_char(sign_1, shift_amount)) + shifted_1;
    }

    static void main() {
        System.out.println(logical_left_shift(17, 2));
        System.out.println(logical_right_shift(1983, 4));
        System.out.println(arithmetic_right_shift(-17, 2));
    }
    public static void main(String[] args) {
        main();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static <T> T[] concat(T[] a, T[] b) {
        T[] out = java.util.Arrays.copyOf(a, a.length + b.length);
        System.arraycopy(b, 0, out, a.length, b.length);
        return out;
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
