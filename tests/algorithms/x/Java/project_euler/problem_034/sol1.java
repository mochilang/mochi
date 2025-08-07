public class Main {
    static int[] DIGIT_FACTORIALS;

    static int sum_of_digit_factorial(int n) {
        if (n == 0) {
            return DIGIT_FACTORIALS[0];
        }
        int total = 0;
        int num = n;
        while (num > 0) {
            int digit = Math.floorMod(num, 10);
            total = total + DIGIT_FACTORIALS[digit];
            num = Math.floorDiv(num, 10);
        }
        return total;
    }

    static int solution() {
        int limit = 7 * DIGIT_FACTORIALS[9] + 1;
        int total_1 = 0;
        int i = 3;
        while (i < limit) {
            if (sum_of_digit_factorial(i) == i) {
                total_1 = total_1 + i;
            }
            i = i + 1;
        }
        return total_1;
    }
    public static void main(String[] args) {
        DIGIT_FACTORIALS = ((int[])(new int[]{1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880}));
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
