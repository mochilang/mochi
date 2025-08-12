public class Main {

    static boolean is_luhn(String s) {
        long n = _runeLen(s);
        if (n <= 1) {
            return false;
        }
        long check_digit_1 = Integer.parseInt(_substr(s, (int)((long)(n - 1)), (int)((long)(n)))));
        long i_1 = n - 2;
        boolean even_1 = true;
        while (i_1 >= 0) {
            long digit_1 = Integer.parseInt(_substr(s, (int)((long)(i_1)), (int)((long)(i_1 + 1)))));
            if (even_1) {
                long doubled_1 = digit_1 * 2;
                if (doubled_1 > 9) {
                    doubled_1 = doubled_1 - 9;
                }
                check_digit_1 = check_digit_1 + doubled_1;
            } else {
                check_digit_1 = check_digit_1 + digit_1;
            }
            even_1 = !even_1;
            i_1 = i_1 - 1;
        }
        return Math.floorMod(check_digit_1, 10) == 0;
    }
    public static void main(String[] args) {
        json(is_luhn("79927398713"));
        json(is_luhn("79927398714"));
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int len = _runeLen(s);
        if (i < 0) i = 0;
        if (j > len) j = len;
        if (i > j) i = j;
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
}
