public class Main {

    static int solution() {
        int a = 1;
        while (a < 999) {
            int b = a;
            while (b < 999) {
                int c = 1000 - a - b;
                if (a * a + b * b == c * c) {
                    return a * b * c;
                }
                b = b + 1;
            }
            a = a + 1;
        }
        return -1;
    }
    public static void main(String[] args) {
        System.out.println(_p(solution()));
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
