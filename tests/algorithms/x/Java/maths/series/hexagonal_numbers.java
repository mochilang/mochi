public class Main {

    static long[] hexagonal_numbers(long length) {
        if ((long)(length) <= 0L) {
            throw new RuntimeException(String.valueOf("Length must be a positive integer."));
        }
        long[] res_1 = ((long[])(new long[]{}));
        long n_1 = 0L;
        while ((long)(n_1) < (long)(length)) {
            res_1 = ((long[])(appendLong(res_1, (long)((long)(n_1) * (long)(((long)(2L * (long)(n_1)) - 1L))))));
            n_1 = (long)((long)(n_1) + 1L);
        }
        return res_1;
    }

    static void test_hexagonal_numbers() {
        long[] expected5 = ((long[])(new long[]{0, 1, 6, 15, 28}));
        long[] result5_1 = ((long[])(hexagonal_numbers(5L)));
        if (!java.util.Arrays.equals(result5_1, expected5)) {
            throw new RuntimeException(String.valueOf("hexagonal_numbers(5) failed"));
        }
        long[] expected10_1 = ((long[])(new long[]{0, 1, 6, 15, 28, 45, 66, 91, 120, 153}));
        long[] result10_1 = ((long[])(hexagonal_numbers(10L)));
        if (!java.util.Arrays.equals(result10_1, expected10_1)) {
            throw new RuntimeException(String.valueOf("hexagonal_numbers(10) failed"));
        }
    }
    public static void main(String[] args) {
        test_hexagonal_numbers();
        System.out.println(_p(hexagonal_numbers(5L)));
        System.out.println(_p(hexagonal_numbers(10L)));
    }

    static long[] appendLong(long[] arr, long v) {
        long[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
