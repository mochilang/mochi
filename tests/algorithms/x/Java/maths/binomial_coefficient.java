public class Main {

    static long binomial_coefficient(long n, long r) {
        if (n < 0 || r < 0) {
            throw new RuntimeException(String.valueOf("n and r must be non-negative integers"));
        }
        if (n == 0 || r == 0) {
            return 1;
        }
        long[] c_1 = ((long[])(new long[]{}));
        for (int _v = 0; _v < (r + 1); _v++) {
            c_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(c_1), java.util.stream.LongStream.of(0)).toArray()));
        }
c_1[(int)(0)] = 1;
        long i_1 = 1;
        while (i_1 <= n) {
            long j_1 = i_1 < r ? i_1 : r;
            while (j_1 > 0) {
c_1[(int)(j_1)] = c_1[(int)(j_1)] + c_1[(int)(j_1 - 1)];
                j_1 = j_1 - 1;
            }
            i_1 = i_1 + 1;
        }
        return c_1[(int)(r)];
    }
    public static void main(String[] args) {
        System.out.println(_p(binomial_coefficient(10, 5)));
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
