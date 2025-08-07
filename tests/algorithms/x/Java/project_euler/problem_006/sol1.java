public class Main {

    static int solution(int n) {
        int sum_of_squares = 0;
        int sum_of_ints = 0;
        int i = 1;
        while (i <= n) {
            sum_of_squares = sum_of_squares + i * i;
            sum_of_ints = sum_of_ints + i;
            i = i + 1;
        }
        return sum_of_ints * sum_of_ints - sum_of_squares;
    }

    static void main() {
        System.out.println(_p(solution(100)));
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
