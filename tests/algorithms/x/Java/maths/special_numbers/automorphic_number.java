public class Main {

    static boolean is_automorphic_number(long number) {
        if ((long)(number) < 0L) {
            return false;
        }
        long n_1 = (long)(number);
        long sq_1 = (long)((long)(number) * (long)(number));
        while ((long)(n_1) > 0L) {
            if (Math.floorMod(n_1, 10) != Math.floorMod(sq_1, 10)) {
                return false;
            }
            n_1 = Math.floorDiv(n_1, 10);
            sq_1 = Math.floorDiv(sq_1, 10);
        }
        return true;
    }
    public static void main(String[] args) {
        System.out.println(_p(is_automorphic_number(0L)));
        System.out.println(_p(is_automorphic_number(1L)));
        System.out.println(_p(is_automorphic_number(5L)));
        System.out.println(_p(is_automorphic_number(6L)));
        System.out.println(_p(is_automorphic_number(7L)));
        System.out.println(_p(is_automorphic_number(25L)));
        System.out.println(_p(is_automorphic_number(376L)));
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
