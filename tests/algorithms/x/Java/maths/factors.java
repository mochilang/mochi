public class Main {

    static long[] reverse(long[] xs) {
        long[] res = ((long[])(new long[]{}));
        long i_1 = (long)((long)(xs.length) - 1L);
        while ((long)(i_1) >= 0L) {
            res = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res), java.util.stream.LongStream.of((long)(xs[(int)((long)(i_1))]))).toArray()));
            i_1 = (long)((long)(i_1) - 1L);
        }
        return res;
    }

    static long[] factors_of_a_number(long num) {
        long[] facs = ((long[])(new long[]{}));
        if ((long)(num) < 1L) {
            return facs;
        }
        long[] small_1 = ((long[])(new long[]{}));
        long[] large_1 = ((long[])(new long[]{}));
        long i_3 = 1L;
        while ((long)((long)(i_3) * (long)(i_3)) <= (long)(num)) {
            if (Math.floorMod(num, i_3) == 0L) {
                small_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(small_1), java.util.stream.LongStream.of((long)(i_3))).toArray()));
                Object d_1 = Math.floorDiv(((long)(num)), ((long)(i_3)));
                if (((Number)(d_1)).intValue() != (long)(i_3)) {
                    large_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(large_1), java.util.stream.LongStream.of(((Number)(d_1)).longValue())).toArray()));
                }
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
        facs = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(small_1), java.util.Arrays.stream(reverse(((long[])(large_1))))).toArray()));
        return facs;
    }

    static void run_tests() {
        if (!java.util.Arrays.equals(factors_of_a_number(1L), new long[]{1})) {
            throw new RuntimeException(String.valueOf("case1 failed"));
        }
        if (!java.util.Arrays.equals(factors_of_a_number(5L), new long[]{1, 5})) {
            throw new RuntimeException(String.valueOf("case2 failed"));
        }
        if (!java.util.Arrays.equals(factors_of_a_number(24L), new long[]{1, 2, 3, 4, 6, 8, 12, 24})) {
            throw new RuntimeException(String.valueOf("case3 failed"));
        }
        if (!java.util.Arrays.equals(factors_of_a_number((long)(-24)), new long[]{})) {
            throw new RuntimeException(String.valueOf("case4 failed"));
        }
    }

    static void main() {
        run_tests();
        System.out.println(_p(factors_of_a_number(24L)));
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
