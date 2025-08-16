public class Main {

    static long decimal_to_negative_base_2(long num) {
        if ((long)(num) == 0L) {
            return 0;
        }
        long n_1 = (long)(num);
        String ans_1 = "";
        while ((long)(n_1) != 0L) {
            long rem_1 = Math.floorMod(n_1, (-2));
            n_1 = Math.floorDiv(n_1, (-2));
            if ((long)(rem_1) < 0L) {
                rem_1 = (long)((long)(rem_1) + 2L);
                n_1 = (long)((long)(n_1) + 1L);
            }
            ans_1 = _p(rem_1) + ans_1;
        }
        return Integer.parseInt(ans_1);
    }
    public static void main(String[] args) {
        System.out.println(decimal_to_negative_base_2(0L));
        System.out.println(decimal_to_negative_base_2((long)(-19)));
        System.out.println(decimal_to_negative_base_2(4L));
        System.out.println(decimal_to_negative_base_2(7L));
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
