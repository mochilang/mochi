public class Main {

    static long factorial(long digit) {
        if ((long)(digit) == 0L || (long)(digit) == 1L) {
            return 1;
        }
        return (long)(digit) * (long)(factorial((long)((long)(digit) - 1L)));
    }

    static boolean is_krishnamurthy(long n) {
        long duplicate = (long)(n);
        long fact_sum_1 = 0L;
        while ((long)(duplicate) > 0L) {
            long digit_1 = Math.floorMod(duplicate, 10);
            fact_sum_1 = (long)((long)(fact_sum_1) + (long)(factorial((long)(digit_1))));
            duplicate = Math.floorDiv(duplicate, 10);
        }
        return (long)(fact_sum_1) == (long)(n);
    }
    public static void main(String[] args) {
        System.out.println(_p(is_krishnamurthy(145L)));
        System.out.println(_p(is_krishnamurthy(240L)));
        System.out.println(_p(is_krishnamurthy(1L)));
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
