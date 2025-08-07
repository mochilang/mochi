public class Main {

    static int solution(int n) {
        int total = 1;
        int i = 1;
        int limit = Math.floorDiv((n + 1), 2);
        while (i < limit) {
            int odd = 2 * i + 1;
            int even = 2 * i;
            total = total + 4 * odd * odd - 6 * even;
            i = i + 1;
        }
        return total;
    }
    public static void main(String[] args) {
        System.out.println(_p(solution(1001)));
        System.out.println(_p(solution(500)));
        System.out.println(_p(solution(100)));
        System.out.println(_p(solution(50)));
        System.out.println(_p(solution(10)));
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
