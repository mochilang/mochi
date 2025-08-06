public class Main {

    static String repeat_char(String ch, int times) {
        String res = "";
        int i = 0;
        while (i < times) {
            res = res + ch;
            i = i + 1;
        }
        return res;
    }

    static String to_binary(int n) {
        if (n == 0) {
            return "0";
        }
        String res_1 = "";
        int v = n;
        while (v > 0) {
            res_1 = _p(Math.floorMod(v, 2)) + res_1;
            v = v / 2;
        }
        return res_1;
    }

    static int pow2(int exp) {
        int res_2 = 1;
        int i_1 = 0;
        while (i_1 < exp) {
            res_2 = res_2 * 2;
            i_1 = i_1 + 1;
        }
        return res_2;
    }

    static String twos_complement(int number) {
        if (number > 0) {
            throw new RuntimeException(String.valueOf("input must be a negative integer"));
        }
        if (number == 0) {
            return "0b0";
        }
        int abs_number = number < 0 ? -number : number;
        int binary_number_length = String.valueOf(to_binary(abs_number)).length();
        int complement_value = pow2(binary_number_length) - abs_number;
        String complement_binary = String.valueOf(to_binary(complement_value));
        String padding = String.valueOf(repeat_char("0", binary_number_length - _runeLen(complement_binary)));
        String twos_complement_number = "1" + padding + complement_binary;
        return "0b" + twos_complement_number;
    }
    public static void main(String[] args) {
        System.out.println(twos_complement(0));
        System.out.println(twos_complement(-1));
        System.out.println(twos_complement(-5));
        System.out.println(twos_complement(-17));
        System.out.println(twos_complement(-207));
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
