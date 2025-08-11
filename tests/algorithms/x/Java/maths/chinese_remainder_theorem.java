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
        if (b == 0) {
            return new EuclidResult(1, 0);
        }
        EuclidResult res_1 = extended_euclid(b, Math.floorMod(a, b));
        long k_1 = Math.floorDiv(a, b);
        return new EuclidResult(res_1.y, res_1.x - k_1 * res_1.y);
    }

    static long chinese_remainder_theorem(long n1, long r1, long n2, long r2) {
        EuclidResult res_2 = extended_euclid(n1, n2);
        long x_1 = res_2.x;
        long y_1 = res_2.y;
        long m_1 = n1 * n2;
        long n_1 = r2 * x_1 * n1 + r1 * y_1 * n2;
        return Math.floorMod(((Math.floorMod(n_1, m_1)) + m_1), m_1);
    }

    static long invert_modulo(long a, long n) {
        EuclidResult res_3 = extended_euclid(a, n);
        long b_1 = res_3.x;
        if (b_1 < 0) {
            b_1 = Math.floorMod((Math.floorMod(b_1, n) + n), n);
        }
        return b_1;
    }

    static long chinese_remainder_theorem2(long n1, long r1, long n2, long r2) {
        long x_2 = invert_modulo(n1, n2);
        long y_3 = invert_modulo(n2, n1);
        long m_3 = n1 * n2;
        long n_3 = r2 * x_2 * n1 + r1 * y_3 * n2;
        return Math.floorMod(((Math.floorMod(n_3, m_3)) + m_3), m_3);
    }
    public static void main(String[] args) {
        e1 = extended_euclid(10, 6);
        System.out.println(_p(e1.x) + "," + _p(e1.y));
        e2 = extended_euclid(7, 5);
        System.out.println(_p(e2.x) + "," + _p(e2.y));
        System.out.println(_p(chinese_remainder_theorem(5, 1, 7, 3)));
        System.out.println(_p(chinese_remainder_theorem(6, 1, 4, 3)));
        System.out.println(_p(invert_modulo(2, 5)));
        System.out.println(_p(invert_modulo(8, 7)));
        System.out.println(_p(chinese_remainder_theorem2(5, 1, 7, 3)));
        System.out.println(_p(chinese_remainder_theorem2(6, 1, 4, 3)));
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
