public class Main {

    static int solution(int n) {
        int a = 0;
        int b = 1;
        int total = 0;
        while (b <= n) {
            if (Math.floorMod(b, 2) == 0) {
                total = total + b;
            }
            int next_val = a + b;
            a = b;
            b = next_val;
        }
        return total;
    }
    public static void main(String[] args) {
        System.out.println(_p(solution(10)));
        System.out.println(_p(solution(15)));
        System.out.println(_p(solution(2)));
        System.out.println(_p(solution(1)));
        System.out.println(_p(solution(34)));
        System.out.println(_p(solution(4000000)));
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
