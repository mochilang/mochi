public class Main {

    static int solution() {
        int[] targets = ((int[])(new int[]{1, 10, 100, 1000, 10000, 100000, 1000000}));
        int idx = 0;
        int product = 1;
        int count = 0;
        int i = 1;
        while (idx < targets.length) {
            String s = _p(i);
            int j = 0;
            while (j < _runeLen(s)) {
                count = count + 1;
                if (count == targets[idx]) {
                    product = product * (s.substring(j, j+1));
                    idx = idx + 1;
                    if (idx == targets.length) {
                        break;
                    }
                }
                j = j + 1;
            }
            i = i + 1;
        }
        return product;
    }

    static void test_solution() {
        if (solution() != 210) {
            throw new RuntimeException(String.valueOf("solution failed"));
        }
    }

    static void main() {
        test_solution();
        System.out.println(_p(solution()));
    }
    public static void main(String[] args) {
        main();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
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
