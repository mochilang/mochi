public class Main {

    static long combinations(long n, long k) {
        if ((long)(k) < 0L || (long)(n) < (long)(k)) {
            throw new RuntimeException(String.valueOf("Please enter positive integers for n and k where n >= k"));
        }
        long res_1 = 1L;
        long i_1 = 0L;
        while ((long)(i_1) < (long)(k)) {
            res_1 = (long)((long)(res_1) * (long)(((long)(n) - (long)(i_1))));
            res_1 = Math.floorDiv(((long)(res_1)), ((long)(((long)(i_1) + 1L))));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return res_1;
    }
    public static void main(String[] args) {
        System.out.println("The number of five-card hands possible from a standard fifty-two card deck is: " + _p(combinations(52L, 5L)));
        System.out.println("");
        System.out.println("If a class of 40 students must be arranged into groups of 4 for group projects, there are " + _p(combinations(40L, 4L)) + " ways to arrange them.");
        System.out.println("");
        System.out.println("If 10 teams are competing in a Formula One race, there are " + _p(combinations(10L, 3L)) + " ways that first, second and third place can be awarded.");
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
