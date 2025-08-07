public class Main {

    static int factorial(int n) {
        int result = 1;
        int i = 2;
        while (i <= n) {
            result = result * i;
            i = i + 1;
        }
        return result;
    }

    static int solution(int n) {
        int total = 2 * n;
        int k = Math.floorDiv(total, 2);
        return Math.floorDiv(factorial(total), (factorial(k) * factorial(total - k)));
    }
    public static void main(String[] args) {
        System.out.println(_p(solution(25)));
        System.out.println(_p(solution(23)));
        System.out.println(_p(solution(20)));
        System.out.println(_p(solution(15)));
        System.out.println(_p(solution(1)));
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
