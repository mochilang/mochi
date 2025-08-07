public class Main {

    static int gcd(int a, int b) {
        int x = a;
        int y = b;
        while (y != 0) {
            int t = y;
            y = Math.floorMod(x, y);
            x = t;
        }
        return x;
    }

    static int lcm(int x, int y) {
        return Math.floorDiv((x * y), gcd(x, y));
    }

    static int solution(int n) {
        int g = 1;
        int i = 1;
        while (i <= n) {
            g = lcm(g, i);
            i = i + 1;
        }
        return g;
    }

    static void main() {
        System.out.println(_p(solution(20)));
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
        return String.valueOf(v);
    }
}
