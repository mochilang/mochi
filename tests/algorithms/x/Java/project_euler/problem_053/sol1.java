public class Main {

    static boolean combination_exceeds(int n, int r, int limit) {
        int r2 = r;
        if (r2 > n - r2) {
            r2 = n - r2;
        }
        int result = 1;
        int k = 1;
        while (k <= r2) {
            result = Math.floorDiv(result * (n - r2 + k), k);
            if (result > limit) {
                return true;
            }
            k = k + 1;
        }
        return result > limit;
    }

    static int count_exceeding(int limit) {
        int total = 0;
        int n = 1;
        while (n <= 100) {
            int r = 1;
            while (r <= n) {
                if (((Boolean)(combination_exceeds(n, r, limit)))) {
                    total = total + 1;
                }
                r = r + 1;
            }
            n = n + 1;
        }
        return total;
    }
    public static void main(String[] args) {
        System.out.println(_p(count_exceeding(1000000)));
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
        return String.valueOf(v);
    }
}
