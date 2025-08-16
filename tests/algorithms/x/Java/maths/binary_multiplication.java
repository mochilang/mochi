public class Main {

    static long binary_multiply(long a, long b) {
        long x = (long)(a);
        long y_1 = (long)(b);
        long res_1 = 0L;
        while ((long)(y_1) > 0L) {
            if (Math.floorMod(y_1, 2) == 1L) {
                res_1 = (long)((long)(res_1) + (long)(x));
            }
            x = (long)((long)(x) + (long)(x));
            y_1 = (long)(((Number)((Math.floorDiv(y_1, 2)))).intValue());
        }
        return res_1;
    }

    static long binary_mod_multiply(long a, long b, long modulus) {
        long x_1 = (long)(a);
        long y_3 = (long)(b);
        long res_3 = 0L;
        while ((long)(y_3) > 0L) {
            if (Math.floorMod(y_3, 2) == 1L) {
                res_3 = Math.floorMod(((long)((Math.floorMod(res_3, modulus))) + (long)((Math.floorMod(x_1, modulus)))), modulus);
            }
            x_1 = (long)((long)(x_1) + (long)(x_1));
            y_3 = (long)(((Number)((Math.floorDiv(y_3, 2)))).intValue());
        }
        return Math.floorMod(res_3, modulus);
    }

    static void main() {
        System.out.println(_p(binary_multiply(2L, 3L)));
        System.out.println(_p(binary_multiply(5L, 0L)));
        System.out.println(_p(binary_mod_multiply(2L, 3L, 5L)));
        System.out.println(_p(binary_mod_multiply(10L, 5L, 13L)));
    }
    public static void main(String[] args) {
        main();
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
