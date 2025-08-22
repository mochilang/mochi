public class Main {
    static long seed = 1L;
    static long[] integers = ((long[])(new long[]{0, 1, 2, 3, 4, 5, 6, 7}));
    static String[] strings = ((String[])(new String[]{"python", "says", "hello", "!"}));

    static long rand() {
        seed = (long)(((long)(Math.floorMod(((long)(((long)((long)(seed) * 1103515245L) + 12345L))), 2147483648L))));
        return ((long)(Math.floorDiv(seed, 65536)));
    }

    static long randint(long a, long b) {
        long r = (long)(rand());
        return (long)(a) + Math.floorMod(r, ((long)((long)(b) - (long)(a)) + 1L));
    }

    static long[] fisher_yates_shuffle_int(long[] data) {
        long[] res = ((long[])(data));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(res.length)) {
            long a_1 = (long)(randint(0L, (long)((long)(res.length) - 1L)));
            long b_1 = (long)(randint(0L, (long)((long)(res.length) - 1L)));
            long temp_1 = (long)(res[(int)((long)(a_1))]);
res[(int)((long)(a_1))] = (long)(res[(int)((long)(b_1))]);
res[(int)((long)(b_1))] = (long)(temp_1);
            i_1 = (long)((long)(i_1) + 1L);
        }
        return res;
    }

    static String[] fisher_yates_shuffle_str(String[] data) {
        String[] res_1 = ((String[])(data));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(res_1.length)) {
            long a_3 = (long)(randint(0L, (long)((long)(res_1.length) - 1L)));
            long b_3 = (long)(randint(0L, (long)((long)(res_1.length) - 1L)));
            String temp_3 = res_1[(int)((long)(a_3))];
res_1[(int)((long)(a_3))] = res_1[(int)((long)(b_3))];
res_1[(int)((long)(b_3))] = temp_3;
            i_3 = (long)((long)(i_3) + 1L);
        }
        return res_1;
    }
    public static void main(String[] args) {
        System.out.println("Fisher-Yates Shuffle:");
        System.out.println("List " + _p(integers) + " " + _p(strings));
        System.out.println("FY Shuffle " + _p(fisher_yates_shuffle_int(((long[])(integers)))) + " " + _p(fisher_yates_shuffle_str(((String[])(strings)))));
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
