public class Main {

    static String pow_string(long base, long exp) {
        if ((long)(exp) >= 0L) {
            long res = 1L;
            long i = 0L;
            while ((long)(i) < (long)(exp)) {
                res = (long)((long)(res) * (long)(base));
                i = (long)((long)(i) + 1L);
            }
            return _p(res);
        }
        long e_1 = (long)(-exp);
        double res_2 = (double)(1.0);
        double b_1 = (double)((double)(base) * (double)(1.0));
        long i_2 = 0L;
        while ((long)(i_2) < (long)(e_1)) {
            res_2 = (double)((double)(res_2) * (double)(b_1));
            i_2 = (long)((long)(i_2) + 1L);
        }
        double value_1 = (double)((double)(1.0) / (double)(res_2));
        return _p(value_1);
    }

    static String[] p_series(long nth_term, long power) {
        String[] series = ((String[])(new String[]{}));
        if ((long)(nth_term) <= 0L) {
            return series;
        }
        long i_4 = 1L;
        while ((long)(i_4) <= (long)(nth_term)) {
            if ((long)(i_4) == 1L) {
                series = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(series), java.util.stream.Stream.of("1")).toArray(String[]::new)));
            } else {
                series = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(series), java.util.stream.Stream.of("1 / " + String.valueOf(pow_string((long)(i_4), (long)(power))))).toArray(String[]::new)));
            }
            i_4 = (long)((long)(i_4) + 1L);
        }
        return series;
    }
    public static void main(String[] args) {
        System.out.println(p_series(5L, 2L));
        System.out.println(p_series((long)(-5), 2L));
        System.out.println(p_series(5L, (long)(-2)));
        System.out.println(p_series(0L, 0L));
        System.out.println(p_series(1L, 1L));
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
