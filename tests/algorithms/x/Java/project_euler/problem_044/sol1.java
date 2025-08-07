public class Main {
    static int result;

    static double sqrt(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess = x;
        int i = 0;
        while (i < 10) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static boolean is_pentagonal(int n) {
        double root = sqrt(1.0 + 24.0 * (1.0 * n));
        double val = (1.0 + root) / 6.0;
        int val_int = ((Number)(val)).intValue();
        return val == (1.0 * val_int);
    }

    static int pentagonal(int k) {
        return Math.floorDiv((k * (3 * k - 1)), 2);
    }

    static int solution(int limit) {
        int[] pentagonal_nums = ((int[])(new int[]{}));
        int i_1 = 1;
        while (i_1 < limit) {
            pentagonal_nums = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(pentagonal_nums), java.util.stream.IntStream.of(pentagonal(i_1))).toArray()));
            i_1 = i_1 + 1;
        }
        int a_idx = 0;
        while (a_idx < pentagonal_nums.length) {
            int pentagonal_i = pentagonal_nums[a_idx];
            int b_idx = a_idx;
            while (b_idx < pentagonal_nums.length) {
                int pentagonal_j = pentagonal_nums[b_idx];
                int s = pentagonal_i + pentagonal_j;
                int d = pentagonal_j - pentagonal_i;
                if (((Boolean)(is_pentagonal(s))) && ((Boolean)(is_pentagonal(d)))) {
                    return d;
                }
                b_idx = b_idx + 1;
            }
            a_idx = a_idx + 1;
        }
        return -1;
    }
    public static void main(String[] args) {
        result = solution(5000);
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
