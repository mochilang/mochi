public class Main {

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

    static String binary_and(int a, int b) {
        if (a < 0 || b < 0) {
            throw new RuntimeException(String.valueOf("the value of both inputs must be positive"));
        }
        String a_bin = String.valueOf(to_binary(a));
        String b_bin = String.valueOf(to_binary(b));
        int max_len = _runeLen(a_bin);
        if (_runeLen(b_bin) > max_len) {
            max_len = _runeLen(b_bin);
        }
        String a_pad = String.valueOf(zfill(a_bin, max_len));
        String b_pad = String.valueOf(zfill(b_bin, max_len));
        int i = 0;
        String res_2 = "";
        while (i < max_len) {
            if ((a_pad.substring(i, i+1).equals("1")) && (b_pad.substring(i, i+1).equals("1"))) {
                res_2 = res_2 + "1";
            } else {
                res_2 = res_2 + "0";
            }
            i = i + 1;
        }
        return "0b" + res_2;
    }
    public static void main(String[] args) {
        System.out.println(binary_and(25, 32));
        System.out.println(binary_and(37, 50));
        System.out.println(binary_and(21, 30));
        System.out.println(binary_and(58, 73));
        System.out.println(binary_and(0, 255));
        System.out.println(binary_and(256, 256));
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
