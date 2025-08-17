public class Main {

    static String[] harmonic_series(double n_term) {
        if ((double)(n_term) <= (double)(0.0)) {
            return new String[]{};
        }
        long limit_1 = (long)(((Number)(n_term)).intValue());
        String[] series_1 = ((String[])(new String[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(limit_1)) {
            if ((long)(i_1) == 0L) {
                series_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(series_1), java.util.stream.Stream.of("1")).toArray(String[]::new)));
            } else {
                series_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(series_1), java.util.stream.Stream.of("1/" + _p((long)(i_1) + 1L))).toArray(String[]::new)));
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return series_1;
    }
    public static void main(String[] args) {
        System.out.println(_p(harmonic_series((double)(5.0))));
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
