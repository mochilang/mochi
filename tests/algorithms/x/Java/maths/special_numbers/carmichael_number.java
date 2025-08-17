public class Main {

    static long abs_int(long x) {
        if ((long)(x) < 0L) {
            return -x;
        }
        return x;
    }

    static long gcd(long a, long b) {
        if ((long)(a) == 0L) {
            return abs_int((long)(b));
        }
        return gcd(Math.floorMod(b, a), (long)(a));
    }

    static long power(long x, long y, long m) {
        if ((long)(y) == 0L) {
            return Math.floorMod(1, m);
        }
        long temp_1 = Math.floorMod(power((long)(x), Math.floorDiv(((long)(y)), ((long)(2))), (long)(m)), m);
        temp_1 = Math.floorMod(((long)(temp_1) * (long)(temp_1)), m);
        if (Math.floorMod(y, 2) == 1L) {
            temp_1 = Math.floorMod(((long)(temp_1) * (long)(x)), m);
        }
        return temp_1;
    }

    static boolean is_carmichael_number(long n) {
        if ((long)(n) <= 0L) {
            throw new RuntimeException(String.valueOf("Number must be positive"));
        }
        long b_1 = 2L;
        while ((long)(b_1) < (long)(n)) {
            if ((long)(gcd((long)(b_1), (long)(n))) == 1L) {
                if ((long)(power((long)(b_1), (long)((long)(n) - 1L), (long)(n))) != 1L) {
                    return false;
                }
            }
            b_1 = (long)((long)(b_1) + 1L);
        }
        return true;
    }
    public static void main(String[] args) {
        System.out.println(_p(power(2L, 15L, 3L)));
        System.out.println(_p(power(5L, 1L, 30L)));
        System.out.println(_p(is_carmichael_number(4L)));
        System.out.println(_p(is_carmichael_number(561L)));
        System.out.println(_p(is_carmichael_number(562L)));
        System.out.println(_p(is_carmichael_number(1105L)));
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
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
