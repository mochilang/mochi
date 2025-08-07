public class Main {

    static int solution() {
        int a = 0;
        while (a < 300) {
            int b = a + 1;
            while (b < 400) {
                int c = b + 1;
                while (c < 500) {
                    if (a + b + c == 1000 && a * a + b * b == c * c) {
                        return a * b * c;
                    }
                    c = c + 1;
                }
                b = b + 1;
            }
            a = a + 1;
        }
        return -1;
    }

    static int solution_fast() {
        int a_1 = 0;
        while (a_1 < 300) {
            int b_1 = 0;
            while (b_1 < 400) {
                int c_1 = 1000 - a_1 - b_1;
                if (a_1 < b_1 && b_1 < c_1 && a_1 * a_1 + b_1 * b_1 == c_1 * c_1) {
                    return a_1 * b_1 * c_1;
                }
                b_1 = b_1 + 1;
            }
            a_1 = a_1 + 1;
        }
        return -1;
    }
    public static void main(String[] args) {
        System.out.println(_p(solution_fast()));
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
