public class Main {

    static long polygonal_num(long n, long sides) {
        if ((long)(n) < 0L || (long)(sides) < 3L) {
            throw new RuntimeException(String.valueOf("Invalid input: num must be >= 0 and sides must be >= 3."));
        }
        long term1_1 = (long)((long)((long)(((long)(sides) - 2L)) * (long)(n)) * (long)(n));
        long term2_1 = (long)((long)(((long)(sides) - 4L)) * (long)(n));
        return ((long)(Math.floorDiv(((long)(term1_1) - (long)(term2_1)), 2)));
    }

    static void main() {
        long n = 5L;
        long sides_1 = 4L;
        long result_1 = (long)(polygonal_num((long)(n), (long)(sides_1)));
        System.out.println(_p(result_1));
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
