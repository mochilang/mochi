public class Main {

    static long[] find_minimum_change(long[] denominations, long value) {
        if (value <= 0) {
            return new long[]{};
        }
        long total_1 = value;
        long[] answer_1 = ((long[])(new long[]{}));
        long i_1 = denominations.length - 1;
        while (i_1 >= 0) {
            long denom_1 = denominations[(int)((long)(i_1))];
            while (total_1 >= denom_1) {
                total_1 = total_1 - denom_1;
                answer_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(answer_1), java.util.stream.LongStream.of(denom_1)).toArray()));
            }
            i_1 = i_1 - 1;
        }
        return answer_1;
    }
    public static void main(String[] args) {
        System.out.println(_p(find_minimum_change(((long[])(new long[]{1, 2, 5, 10, 20, 50, 100, 500, 2000})), 987L)));
        System.out.println(_p(find_minimum_change(((long[])(new long[]{1, 5, 100, 500, 1000})), 456L)));
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
