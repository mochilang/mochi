public class Main {
    static long MOD_ADLER;

    static long ord(String ch) {
        String lower = "abcdefghijklmnopqrstuvwxyz";
        String upper_1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String digits_1 = "0123456789";
        long i_1 = 0L;
        while (i_1 < _runeLen(lower)) {
            if ((lower.substring((int)((long)(i_1)), (int)((long)(i_1))+1).equals(ch))) {
                return 97 + i_1;
            }
            i_1 = i_1 + 1;
        }
        i_1 = 0;
        while (i_1 < _runeLen(upper_1)) {
            if ((upper_1.substring((int)((long)(i_1)), (int)((long)(i_1))+1).equals(ch))) {
                return 65 + i_1;
            }
            i_1 = i_1 + 1;
        }
        i_1 = 0;
        while (i_1 < _runeLen(digits_1)) {
            if ((digits_1.substring((int)((long)(i_1)), (int)((long)(i_1))+1).equals(ch))) {
                return 48 + i_1;
            }
            i_1 = i_1 + 1;
        }
        if ((ch.equals(" "))) {
            return 32;
        }
        return 0;
    }

    static long adler32(String plain_text) {
        long a = 1L;
        long b_1 = 0L;
        long i_3 = 0L;
        while (i_3 < _runeLen(plain_text)) {
            long code_1 = ord(plain_text.substring((int)((long)(i_3)), (int)((long)(i_3))+1));
            a = Math.floorMod((a + code_1), MOD_ADLER);
            b_1 = Math.floorMod((b_1 + a), MOD_ADLER);
            i_3 = i_3 + 1;
        }
        return b_1 * 65536 + a;
    }

    static void main() {
        System.out.println(_p(adler32("Algorithms")));
        System.out.println(_p(adler32("go adler em all")));
    }
    public static void main(String[] args) {
        MOD_ADLER = 65521;
        main();
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
