public class Main {
    static int result;

    static int solution(int n) {
        int i = 1;
        int j = 2;
        int total = 0;
        while (j <= n) {
            if (Math.floorMod(j, 2) == 0) {
                total = total + j;
            }
            int next = i + j;
            i = j;
            j = next;
        }
        return total;
    }
    public static void main(String[] args) {
        result = solution(4000000);
        System.out.println("solution() = " + _p(result));
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
