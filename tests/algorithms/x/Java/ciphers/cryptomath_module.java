public class Main {

    static int gcd(int a, int b) {
        int x = a < 0 ? -a : a;
        int y = b < 0 ? -b : b;
        while (y != 0) {
            int t = Math.floorMod(x, y);
            x = y;
            y = t;
        }
        return x;
    }

    static int find_mod_inverse(int a, int m) {
        if (gcd(a, m) != 1) {
            throw new RuntimeException(String.valueOf("mod inverse of " + _p(a) + " and " + _p(m) + " does not exist"));
        }
        int u1 = 1;
        int u2 = 0;
        int u3 = a;
        int v1 = 0;
        int v2 = 1;
        int v3 = m;
        while (v3 != 0) {
            int q = u3 / v3;
            int t1 = u1 - q * v1;
            int t2 = u2 - q * v2;
            int t3 = u3 - q * v3;
            u1 = v1;
            u2 = v2;
            u3 = v3;
            v1 = t1;
            v2 = t2;
            v3 = t3;
        }
        int res = Math.floorMod(u1, m);
        if (res < 0) {
            res = res + m;
        }
        return res;
    }
    public static void main(String[] args) {
        System.out.println(_p(find_mod_inverse(3, 11)));
        System.out.println(_p(find_mod_inverse(7, 26)));
        System.out.println(_p(find_mod_inverse(11, 26)));
        System.out.println(_p(find_mod_inverse(17, 43)));
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
