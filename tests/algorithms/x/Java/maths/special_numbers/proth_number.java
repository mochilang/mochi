public class Main {

    static long pow2(long exp) {
        long result = 1L;
        long i_1 = 0L;
        while ((long)(i_1) < (long)(exp)) {
            result = (long)((long)(result) * 2L);
            i_1 = (long)((long)(i_1) + 1L);
        }
        return result;
    }

    static long proth(long number) {
        if ((long)(number) < 1L) {
            throw new RuntimeException(String.valueOf("Input value must be > 0"));
        }
        if ((long)(number) == 1L) {
            return 3;
        }
        if ((long)(number) == 2L) {
            return 5;
        }
        long temp_1 = (long)(((Number)((Math.floorDiv(((long)(number)), ((long)(3)))))).intValue());
        long pow_1 = 1L;
        long block_index_1 = 1L;
        while ((long)(pow_1) <= (long)(temp_1)) {
            pow_1 = (long)((long)(pow_1) * 2L);
            block_index_1 = (long)((long)(block_index_1) + 1L);
        }
        long[] proth_list_1 = ((long[])(new long[]{3, 5}));
        long proth_index_1 = 2L;
        long increment_1 = 3L;
        long block_1 = 1L;
        while ((long)(block_1) < (long)(block_index_1)) {
            long i_3 = 0L;
            while ((long)(i_3) < (long)(increment_1)) {
                long next_val_1 = (long)((long)(pow2((long)((long)(block_1) + 1L))) + (long)(proth_list_1[(int)((long)((long)(proth_index_1) - 1L))]));
                proth_list_1 = ((long[])(appendLong(proth_list_1, (long)(next_val_1))));
                proth_index_1 = (long)((long)(proth_index_1) + 1L);
                i_3 = (long)((long)(i_3) + 1L);
            }
            increment_1 = (long)((long)(increment_1) * 2L);
            block_1 = (long)((long)(block_1) + 1L);
        }
        return proth_list_1[(int)((long)((long)(number) - 1L))];
    }

    static void main() {
        long n = 1L;
        while ((long)(n) <= 10L) {
            long value_1 = (long)(proth((long)(n)));
            System.out.println("The " + _p(n) + "th Proth number: " + _p(value_1));
            n = (long)((long)(n) + 1L);
        }
    }
    public static void main(String[] args) {
        main();
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
