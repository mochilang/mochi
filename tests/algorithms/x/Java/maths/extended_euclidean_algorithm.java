public class Main {
    static class Coeffs {
        long x;
        long y;
        Coeffs(long x, long y) {
            this.x = x;
            this.y = y;
        }
        Coeffs() {}
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s}", String.valueOf(x), String.valueOf(y));
        }
    }


    static long abs_val(long n) {
        if ((long)(n) < 0L) {
            return -n;
        }
        return n;
    }

    static Coeffs extended_euclidean_algorithm(long a, long b) {
        if ((long)(abs_val((long)(a))) == 1L) {
            return new Coeffs(a, 0);
        }
        if ((long)(abs_val((long)(b))) == 1L) {
            return new Coeffs(0, b);
        }
        long old_remainder_1 = (long)(a);
        long remainder_1 = (long)(b);
        long old_coeff_a_1 = 1L;
        long coeff_a_1 = 0L;
        long old_coeff_b_1 = 0L;
        long coeff_b_1 = 1L;
        while ((long)(remainder_1) != 0L) {
            long quotient_1 = Math.floorDiv(((long)(old_remainder_1)), ((long)(remainder_1)));
            long temp_remainder_1 = (long)((long)(old_remainder_1) - (long)((long)(quotient_1) * (long)(remainder_1)));
            old_remainder_1 = (long)(remainder_1);
            remainder_1 = (long)(temp_remainder_1);
            long temp_a_1 = (long)((long)(old_coeff_a_1) - (long)((long)(quotient_1) * (long)(coeff_a_1)));
            old_coeff_a_1 = (long)(coeff_a_1);
            coeff_a_1 = (long)(temp_a_1);
            long temp_b_1 = (long)((long)(old_coeff_b_1) - (long)((long)(quotient_1) * (long)(coeff_b_1)));
            old_coeff_b_1 = (long)(coeff_b_1);
            coeff_b_1 = (long)(temp_b_1);
        }
        if ((long)(a) < 0L) {
            old_coeff_a_1 = (long)(-old_coeff_a_1);
        }
        if ((long)(b) < 0L) {
            old_coeff_b_1 = (long)(-old_coeff_b_1);
        }
        return new Coeffs(old_coeff_a_1, old_coeff_b_1);
    }

    static void test_extended_euclidean_algorithm() {
        Coeffs r1 = extended_euclidean_algorithm(1L, 24L);
        if (((long)(r1.x) != 1L) || ((long)(r1.y) != 0L)) {
            throw new RuntimeException(String.valueOf("test1 failed"));
        }
        Coeffs r2_1 = extended_euclidean_algorithm(8L, 14L);
        if (((long)(r2_1.x) != 2L) || ((long)(r2_1.y) != (long)((-1)))) {
            throw new RuntimeException(String.valueOf("test2 failed"));
        }
        Coeffs r3_1 = extended_euclidean_algorithm(240L, 46L);
        if (((long)(r3_1.x) != (long)((-9))) || ((long)(r3_1.y) != 47L)) {
            throw new RuntimeException(String.valueOf("test3 failed"));
        }
        Coeffs r4_1 = extended_euclidean_algorithm(1L, (long)(-4));
        if (((long)(r4_1.x) != 1L) || ((long)(r4_1.y) != 0L)) {
            throw new RuntimeException(String.valueOf("test4 failed"));
        }
        Coeffs r5_1 = extended_euclidean_algorithm((long)(-2), (long)(-4));
        if (((long)(r5_1.x) != (long)((-1))) || ((long)(r5_1.y) != 0L)) {
            throw new RuntimeException(String.valueOf("test5 failed"));
        }
        Coeffs r6_1 = extended_euclidean_algorithm(0L, (long)(-4));
        if (((long)(r6_1.x) != 0L) || ((long)(r6_1.y) != (long)((-1)))) {
            throw new RuntimeException(String.valueOf("test6 failed"));
        }
        Coeffs r7_1 = extended_euclidean_algorithm(2L, 0L);
        if (((long)(r7_1.x) != 1L) || ((long)(r7_1.y) != 0L)) {
            throw new RuntimeException(String.valueOf("test7 failed"));
        }
    }

    static void main() {
        test_extended_euclidean_algorithm();
        Coeffs res_1 = extended_euclidean_algorithm(240L, 46L);
        System.out.println("(" + _p(res_1.x) + ", " + _p(res_1.y) + ")");
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
