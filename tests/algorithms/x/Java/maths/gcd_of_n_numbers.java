public class Main {

    static long gcd(long a, long b) {
        long x = (long)(a);
        long y_1 = (long)(b);
        while ((long)(y_1) != 0L) {
            long r_1 = Math.floorMod(x, y_1);
            x = (long)(y_1);
            y_1 = (long)(r_1);
        }
        if ((long)(x) < 0L) {
            return -x;
        }
        return x;
    }

    static long get_greatest_common_divisor(long[] nums) {
        if ((long)(nums.length) == 0L) {
            throw new RuntimeException(String.valueOf("at least one number is required"));
        }
        long g_1 = (long)(nums[(int)(0L)]);
        if ((long)(g_1) <= 0L) {
            throw new RuntimeException(String.valueOf("numbers must be integer and greater than zero"));
        }
        long i_1 = 1L;
        while ((long)(i_1) < (long)(nums.length)) {
            long n_1 = (long)(nums[(int)((long)(i_1))]);
            if ((long)(n_1) <= 0L) {
                throw new RuntimeException(String.valueOf("numbers must be integer and greater than zero"));
            }
            g_1 = (long)(gcd((long)(g_1), (long)(n_1)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return g_1;
    }
    public static void main(String[] args) {
        System.out.println(_p(get_greatest_common_divisor(((long[])(new long[]{18, 45})))));
        System.out.println(_p(get_greatest_common_divisor(((long[])(new long[]{23, 37})))));
        System.out.println(_p(get_greatest_common_divisor(((long[])(new long[]{2520, 8350})))));
        System.out.println(_p(get_greatest_common_divisor(((long[])(new long[]{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})))));
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
