public class Main {
    static String ascii;

    static long ord(String ch) {
        long i = 0L;
        while (i < _runeLen(ascii)) {
            if ((_substr(ascii, (int)((long)(i)), (int)((long)(i + 1)))).equals(ch))) {
                return 32 + i;
            }
            i = i + 1;
        }
        return 0;
    }

    static long bit_and(long a, long b) {
        long ua = a;
        long ub_1 = b;
        long res_1 = 0L;
        long bit_1 = 1L;
        while (ua > 0 || ub_1 > 0) {
            if (Math.floorMod(ua, 2) == 1 && Math.floorMod(ub_1, 2) == 1) {
                res_1 = res_1 + bit_1;
            }
            ua = ((Number)((Math.floorDiv(ua, 2)))).intValue();
            ub_1 = ((Number)((Math.floorDiv(ub_1, 2)))).intValue();
            bit_1 = bit_1 * 2;
        }
        return res_1;
    }

    static long bit_xor(long a, long b) {
        long ua_1 = a;
        long ub_3 = b;
        long res_3 = 0L;
        long bit_3 = 1L;
        while (ua_1 > 0 || ub_3 > 0) {
            long abit_1 = Math.floorMod(ua_1, 2);
            long bbit_1 = Math.floorMod(ub_3, 2);
            if (abit_1 != bbit_1) {
                res_3 = res_3 + bit_3;
            }
            ua_1 = ((Number)((Math.floorDiv(ua_1, 2)))).intValue();
            ub_3 = ((Number)((Math.floorDiv(ub_3, 2)))).intValue();
            bit_3 = bit_3 * 2;
        }
        return res_3;
    }

    static long bit_not32(long x) {
        long ux = x;
        long res_5 = 0L;
        long bit_5 = 1L;
        long count_1 = 0L;
        while (count_1 < 32) {
            if (Math.floorMod(ux, 2) == 0) {
                res_5 = res_5 + bit_5;
            }
            ux = ((Number)((Math.floorDiv(ux, 2)))).intValue();
            bit_5 = bit_5 * 2;
            count_1 = count_1 + 1;
        }
        return res_5;
    }

    static long elf_hash(String data) {
        long hash_ = 0L;
        long i_2 = 0L;
        while (i_2 < _runeLen(data)) {
            long c_1 = ord(_substr(data, (int)((long)(i_2)), (int)((long)(i_2 + 1)))));
            hash_ = hash_ * 16 + c_1;
            long x_1 = bit_and(hash_, 4026531840L);
            if (x_1 != 0) {
                hash_ = bit_xor(hash_, ((Number)((Math.floorDiv(x_1, 16777216)))).intValue());
            }
            hash_ = bit_and(hash_, bit_not32(x_1));
            i_2 = i_2 + 1;
        }
        return hash_;
    }
    public static void main(String[] args) {
        ascii = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";
        System.out.println(_p(elf_hash("lorem ipsum")));
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
