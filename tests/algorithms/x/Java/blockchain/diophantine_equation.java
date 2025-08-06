public class Main {
    static int[] s1;
    static int[][] sols_1;
    static int j = 0;

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

    static int[] extended_gcd(int a, int b) {
        if (b == 0) {
            return new int[]{a, 1, 0};
        }
        int[] res = ((int[])(extended_gcd(b, Math.floorMod(a, b))));
        int d = res[0];
        int p = res[1];
        int q = res[2];
        int x_1 = q;
        int y_1 = p - q * (a / b);
        return new int[]{d, x_1, y_1};
    }

    static int[] diophantine(int a, int b, int c) {
        int d_1 = gcd(a, b);
        if (Math.floorMod(c, d_1) != 0) {
            throw new RuntimeException(String.valueOf("No solution"));
        }
        int[] eg = ((int[])(extended_gcd(a, b)));
        int r = c / d_1;
        int x_2 = eg[1] * r;
        int y_2 = eg[2] * r;
        return new int[]{x_2, y_2};
    }

    static int[][] diophantine_all_soln(int a, int b, int c, int n) {
        int[] base = ((int[])(diophantine(a, b, c)));
        int x0 = base[0];
        int y0 = base[1];
        int d_2 = gcd(a, b);
        int p_1 = a / d_2;
        int q_1 = b / d_2;
        int[][] sols = ((int[][])(new int[][]{}));
        int i = 0;
        while (i < n) {
            int x_3 = x0 + i * q_1;
            int y_3 = y0 - i * p_1;
            sols = ((int[][])(appendObj(sols, new int[]{x_3, y_3})));
            i = i + 1;
        }
        return sols;
    }
    public static void main(String[] args) {
        s1 = ((int[])(diophantine(10, 6, 14)));
        System.out.println(_p(s1));
        sols_1 = ((int[][])(diophantine_all_soln(10, 6, 14, 4)));
        j = 0;
        while (j < sols_1.length) {
            System.out.println(_p(_geto(sols_1, j)));
            j = j + 1;
        }
        System.out.println(_p(diophantine(391, 299, -69)));
        System.out.println(_p(extended_gcd(10, 6)));
        System.out.println(_p(extended_gcd(7, 5)));
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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

    static Object _geto(Object[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
