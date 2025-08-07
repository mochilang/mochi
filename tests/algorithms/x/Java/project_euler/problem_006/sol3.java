public class Main {

    static int solution(int n) {
        int i = 1;
        int sum = 0;
        int sum_of_squares = 0;
        while (i <= n) {
            sum = sum + i;
            sum_of_squares = sum_of_squares + i * i;
            i = i + 1;
        }
        int square_of_sum = sum * sum;
        return square_of_sum - sum_of_squares;
    }
    public static void main(String[] args) {
        System.out.println("solution() = " + _p(solution(100)));
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
