public class Main {

    static long catalan(long n) {
        if ((long)(n) < 1L) {
            throw new RuntimeException(String.valueOf("Input value of [number=" + _p(n) + "] must be > 0"));
        }
        long current_1 = 1L;
        long i_1 = 1L;
        while ((long)(i_1) < (long)(n)) {
            current_1 = (long)((long)(current_1) * (long)(((long)(4L * (long)(i_1)) - 2L)));
            current_1 = (long)(((Number)((Math.floorDiv(current_1, ((long)(i_1) + 1L))))).intValue());
            i_1 = (long)((long)(i_1) + 1L);
        }
        return current_1;
    }

    static void main() {
        if ((long)(catalan(1L)) != 1L) {
            throw new RuntimeException(String.valueOf("catalan(1) should be 1"));
        }
        if ((long)(catalan(5L)) != 14L) {
            throw new RuntimeException(String.valueOf("catalan(5) should be 14"));
        }
        System.out.println(_p(catalan(5L)));
    }
    public static void main(String[] args) {
        main();
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
