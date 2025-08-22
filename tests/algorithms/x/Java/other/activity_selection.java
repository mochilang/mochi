public class Main {
    static long[] start = ((long[])(new long[]{1, 3, 0, 5, 8, 5}));
    static long[] finish = ((long[])(new long[]{2, 4, 6, 7, 9, 9}));

    static void print_max_activities(long[] start, long[] finish) {
        long n = (long)(finish.length);
        System.out.println("The following activities are selected:");
        long i_1 = 0L;
        String result_1 = "0,";
        long j_1 = 1L;
        while ((long)(j_1) < (long)(n)) {
            if ((long)(start[(int)((long)(j_1))]) >= (long)(finish[(int)((long)(i_1))])) {
                result_1 = result_1 + _p(j_1) + ",";
                i_1 = (long)(j_1);
            }
            j_1 = (long)((long)(j_1) + 1L);
        }
        System.out.println(result_1);
    }
    public static void main(String[] args) {
        print_max_activities(((long[])(start)), ((long[])(finish)));
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
