public class Main {

    static boolean is_prime(long number) {
        if (1L < (long)(number) && (long)(number) < 4L) {
            return true;
        } else         if ((long)(number) < 2L || Math.floorMod(number, 2) == 0L || Math.floorMod(number, 3) == 0L) {
            return false;
        }
        long i_1 = 5L;
        while ((long)((long)(i_1) * (long)(i_1)) <= (long)(number)) {
            if (Math.floorMod(number, i_1) == 0L || Math.floorMod(number, ((long)(i_1) + 2L)) == 0L) {
                return false;
            }
            i_1 = (long)((long)(i_1) + 6L);
        }
        return true;
    }

    static long solution(long nth) {
        long count = 0L;
        long num_1 = 2L;
        while (true) {
            if (is_prime((long)(num_1))) {
                count = (long)((long)(count) + 1L);
                if ((long)(count) == (long)(nth)) {
                    return num_1;
                }
            }
            num_1 = (long)((long)(num_1) + 1L);
        }
    }
    public static void main(String[] args) {
        System.out.println("solution() = " + _p(solution(10001L)));
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
