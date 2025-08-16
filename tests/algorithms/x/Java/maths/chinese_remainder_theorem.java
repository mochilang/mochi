public class Main {
    static class EuclidResult {
        long x;
        long y;
        EuclidResult(long x, long y) {
            this.x = x;
            this.y = y;
        }
        EuclidResult() {}
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s}", String.valueOf(x), String.valueOf(y));
        }
    }

    static EuclidResult e1;
    static EuclidResult e2;

    static EuclidResult extended_euclid(long a, long b) {
        if ((long)(b) == 0L) {
            return new EuclidResult(1, 0);
        }
        EuclidResult res_1 = extended_euclid((long)(b), Math.floorMod(a, b));
        long k_1 = Math.floorDiv(((long)(a)), ((long)(b)));
        return new EuclidResult(res_1.y, (long)(res_1.x) - (long)((long)(k_1) * (long)(res_1.y)));
    }

    static long chinese_remainder_theorem(long n1, long r1, long n2, long r2) {
        EuclidResult res_2 = extended_euclid((long)(n1), (long)(n2));
        long x_1 = (long)(res_2.x);
        long y_1 = (long)(res_2.y);
        long m_1 = (long)((long)(n1) * (long)(n2));
        long n_1 = (long)((long)((long)((long)(r2) * (long)(x_1)) * (long)(n1)) + (long)((long)((long)(r1) * (long)(y_1)) * (long)(n2)));
        return Math.floorMod(((long)((Math.floorMod(n_1, m_1))) + (long)(m_1)), m_1);
    }

    static long invert_modulo(long a, long n) {
        EuclidResult res_3 = extended_euclid((long)(a), (long)(n));
        long b_1 = (long)(res_3.x);
        if ((long)(b_1) < 0L) {
            b_1 = Math.floorMod((Math.floorMod(b_1, n) + (long)(n)), n);
        }
        return b_1;
    }

    static long chinese_remainder_theorem2(long n1, long r1, long n2, long r2) {
        long x_2 = (long)(invert_modulo((long)(n1), (long)(n2)));
        long y_3 = (long)(invert_modulo((long)(n2), (long)(n1)));
        long m_3 = (long)((long)(n1) * (long)(n2));
        long n_3 = (long)((long)((long)((long)(r2) * (long)(x_2)) * (long)(n1)) + (long)((long)((long)(r1) * (long)(y_3)) * (long)(n2)));
        return Math.floorMod(((long)((Math.floorMod(n_3, m_3))) + (long)(m_3)), m_3);
    }
    public static void main(String[] args) {
        e1 = extended_euclid(10L, 6L);
        System.out.println(_p(e1.x) + "," + _p(e1.y));
        e2 = extended_euclid(7L, 5L);
        System.out.println(_p(e2.x) + "," + _p(e2.y));
        System.out.println(_p(chinese_remainder_theorem(5L, 1L, 7L, 3L)));
        System.out.println(_p(chinese_remainder_theorem(6L, 1L, 4L, 3L)));
        System.out.println(_p(invert_modulo(2L, 5L)));
        System.out.println(_p(invert_modulo(8L, 7L)));
        System.out.println(_p(chinese_remainder_theorem2(5L, 1L, 7L, 3L)));
        System.out.println(_p(chinese_remainder_theorem2(6L, 1L, 4L, 3L)));
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
