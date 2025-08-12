public class Main {
    static long[] start;
    static long[] finish;

    static void print_max_activities(long[] start, long[] finish) {
        long n = (long)(finish.length);
        System.out.println("The following activities are selected:");
        long i_1 = 0L;
        String result_1 = "0,";
        long j_1 = 1L;
        while (j_1 < n) {
            if (start[(int)((long)(j_1))] >= finish[(int)((long)(i_1))]) {
                result_1 = result_1 + _p(j_1) + ",";
                i_1 = j_1;
            }
            j_1 = (long)(j_1 + (long)(1));
        }
        System.out.println(result_1);
    }
    public static void main(String[] args) {
        start = ((long[])(new long[]{1, 3, 0, 5, 8, 5}));
        finish = ((long[])(new long[]{2, 4, 6, 7, 9, 9}));
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
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
