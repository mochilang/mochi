public class Main {

    static int actual_power(int a, int b) {
        if (b == 0) {
            return 1;
        }
        int half = actual_power(a, Math.floorDiv(b, 2));
        if (Math.floorMod(b, 2) == 0) {
            return half * half;
        }
        return a * half * half;
    }

    static double power(int a, int b) {
        if (b < 0) {
            return 1.0 / (1.0 * actual_power(a, -b));
        }
        return 1.0 * actual_power(a, b);
    }
    public static void main(String[] args) {
        System.out.println(_p(power(4, 6)));
        System.out.println(_p(power(2, 3)));
        System.out.println(_p(power(-2, 3)));
        System.out.println(_p(power(2, -3)));
        System.out.println(_p(power(-2, -3)));
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
