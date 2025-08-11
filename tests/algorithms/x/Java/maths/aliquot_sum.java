public class Main {

    static long aliquot_sum(long n) {
        if (n <= 0) {
            throw new RuntimeException(String.valueOf("Input must be positive"));
        }
        long total_1 = 0;
        long divisor_1 = 1;
        while (divisor_1 <= Math.floorDiv(n, 2)) {
            if (Math.floorMod(n, divisor_1) == 0) {
                total_1 = total_1 + divisor_1;
            }
            divisor_1 = divisor_1 + 1;
        }
        return total_1;
    }
    public static void main(String[] args) {
        System.out.println(_p(aliquot_sum(15)));
        System.out.println(_p(aliquot_sum(6)));
        System.out.println(_p(aliquot_sum(12)));
        System.out.println(_p(aliquot_sum(1)));
        System.out.println(_p(aliquot_sum(19)));
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
