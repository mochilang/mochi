public class Main {

    static long get_avg(long number_1, long number_2) {
        return ((long)(Math.floorDiv(((long)(((long)(number_1) + (long)(number_2)))), ((long)(2)))));
    }

    static long[] guess_the_number(long lower, long higher, long to_guess) {
        if ((long)(lower) > (long)(higher)) {
            throw new RuntimeException(String.valueOf("argument value for lower and higher must be(lower > higher)"));
        }
        if (!((long)(lower) < (long)(to_guess) && (long)(to_guess) < (long)(higher))) {
            throw new RuntimeException(String.valueOf("guess value must be within the range of lower and higher value"));
        }
        java.util.function.Function<Long,String>[] answer = new java.util.function.Function[1];
        answer[0] = (number) -> {
        if ((long)(number) > (long)(to_guess)) {
            return "high";
        } else         if ((long)(number) < (long)(to_guess)) {
            return "low";
        } else {
            return "same";
        }
};
        System.out.println("started...");
        long last_lowest_1 = (long)(lower);
        long last_highest_1 = (long)(higher);
        long[] last_numbers_1 = ((long[])(new long[]{}));
        while (true) {
            long number_1 = (long)(get_avg((long)(last_lowest_1), (long)(last_highest_1)));
            last_numbers_1 = ((long[])(appendLong(last_numbers_1, (long)(number_1))));
            String resp_1 = String.valueOf(answer[0].apply((long)(number_1)));
            if ((resp_1.equals("low"))) {
                last_lowest_1 = (long)(number_1);
            } else             if ((resp_1.equals("high"))) {
                last_highest_1 = (long)(number_1);
            } else {
                break;
            }
        }
        System.out.println("guess the number : " + _p(_geti(last_numbers_1, ((Number)((long)(last_numbers_1.length) - 1L)).intValue())));
        System.out.println("details : " + _p(last_numbers_1));
        return last_numbers_1;
    }
    public static void main(String[] args) {
        guess_the_number(10L, 1000L, 17L);
        guess_the_number((long)(-10000), 10000L, 7L);
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

    static Long _geti(long[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
