public class Main {

    static long aliquot_sum(long n) {
        if ((long)(n) <= 0L) {
            throw new RuntimeException(String.valueOf("Input must be positive"));
        }
        long total_1 = 0L;
        long divisor_1 = 1L;
        while ((long)(divisor_1) <= Math.floorDiv(((long)(n)), ((long)(2)))) {
            if (Math.floorMod(n, divisor_1) == 0L) {
                total_1 = (long)((long)(total_1) + (long)(divisor_1));
            }
            divisor_1 = (long)((long)(divisor_1) + 1L);
        }
        return total_1;
    }
    public static void main(String[] args) {
        System.out.println(_p(aliquot_sum(15L)));
        System.out.println(_p(aliquot_sum(6L)));
        System.out.println(_p(aliquot_sum(12L)));
        System.out.println(_p(aliquot_sum(1L)));
        System.out.println(_p(aliquot_sum(19L)));
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
