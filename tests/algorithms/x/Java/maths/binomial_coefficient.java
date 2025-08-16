public class Main {

    static long binomial_coefficient(long n, long r) {
        if ((long)(n) < 0L || (long)(r) < 0L) {
            throw new RuntimeException(String.valueOf("n and r must be non-negative integers"));
        }
        if ((long)(n) == 0L || (long)(r) == 0L) {
            return 1;
        }
        long[] c_1 = ((long[])(new long[]{}));
        for (int _v = 0; _v < ((long)(r) + 1L); _v++) {
            c_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(c_1), java.util.stream.LongStream.of(0L)).toArray()));
        }
c_1[(int)(0L)] = 1L;
        long i_1 = 1L;
        while ((long)(i_1) <= (long)(n)) {
            long j_1 = (long)((long)(i_1) < (long)(r) ? i_1 : r);
            while ((long)(j_1) > 0L) {
c_1[(int)((long)(j_1))] = (long)((long)(c_1[(int)((long)(j_1))]) + (long)(c_1[(int)((long)((long)(j_1) - 1L))]));
                j_1 = (long)((long)(j_1) - 1L);
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return c_1[(int)((long)(r))];
    }
    public static void main(String[] args) {
        System.out.println(_p(binomial_coefficient(10L, 5L)));
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
