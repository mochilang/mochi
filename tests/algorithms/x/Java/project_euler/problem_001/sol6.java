public class Main {

    static int solution(int n) {
        int a = 3;
        int result = 0;
        while (a < n) {
            if (Math.floorMod(a, 3) == 0 || Math.floorMod(a, 5) == 0) {
                result = result + a;
            } else             if (Math.floorMod(a, 15) == 0) {
                result = result - a;
            }
            a = a + 1;
        }
        return result;
    }

    static void main() {
        System.out.println("solution() = " + _p(solution(1000)));
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
