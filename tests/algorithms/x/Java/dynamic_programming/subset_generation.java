public class Main {

    static long[] copy_list(long[] src) {
        long[] result = ((long[])(new long[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(src.length)) {
            result = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(result), java.util.stream.LongStream.of(src[(int)((long)(i_1))])).toArray()));
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        return result;
    }

    static long[][] subset_combinations(long[] elements, long n) {
        long r = (long)(elements.length);
        if (n > (long)(r)) {
            return new long[][]{};
        }
        long[][][] dp_1 = ((long[][][])(new long[][][]{}));
        long i_3 = 0L;
        while ((long)(i_3) <= (long)(r)) {
            dp_1 = ((long[][][])(java.util.stream.Stream.concat(java.util.Arrays.stream(dp_1), java.util.stream.Stream.of(new long[][][]{new long[][]{}})).toArray(long[][][]::new)));
            i_3 = (long)((long)(i_3) + (long)(1));
        }
dp_1[(int)((long)(0))] = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(dp_1[(int)((long)(0))]), java.util.stream.Stream.of(new long[][]{new long[]{}})).toArray(long[][]::new)));
        i_3 = (long)(1);
        while ((long)(i_3) <= (long)(r)) {
            long j_1 = (long)(i_3);
            while ((long)(j_1) > (long)(0)) {
                long[][] prevs_1 = ((long[][])(dp_1[(int)((long)((long)(j_1) - (long)(1)))]));
                long k_1 = 0L;
                while ((long)(k_1) < (long)(prevs_1.length)) {
                    long[] prev_1 = ((long[])(prevs_1[(int)((long)(k_1))]));
                    long[] comb_1 = ((long[])(copy_list(((long[])(prev_1)))));
                    comb_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(comb_1), java.util.stream.LongStream.of(elements[(int)((long)((long)(i_3) - (long)(1)))])).toArray()));
dp_1[(int)((long)(j_1))] = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(dp_1[(int)((long)(j_1))]), java.util.stream.Stream.of(new long[][]{comb_1})).toArray(long[][]::new)));
                    k_1 = (long)((long)(k_1) + (long)(1));
                }
                j_1 = (long)((long)(j_1) - (long)(1));
            }
            i_3 = (long)((long)(i_3) + (long)(1));
        }
        return dp_1[(int)((long)(n))];
    }
    public static void main(String[] args) {
        System.out.println(_p(subset_combinations(((long[])(new long[]{10, 20, 30, 40})), 2L)));
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
