public class Main {

    static int[] make_list(int len, int value) {
        int[] arr = ((int[])(new int[]{}));
        int i = 0;
        while (i < len) {
            arr = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(arr), java.util.stream.IntStream.of(value)).toArray()));
            i = i + 1;
        }
        return arr;
    }

    static int count_recursive(int[] array, int target) {
        if (target < 0) {
            return 0;
        }
        if (target == 0) {
            return 1;
        }
        int total = 0;
        int i_1 = 0;
        while (i_1 < array.length) {
            total = total + count_recursive(((int[])(array)), target - array[i_1]);
            i_1 = i_1 + 1;
        }
        return total;
    }

    static int combination_sum_iv(int[] array, int target) {
        return count_recursive(((int[])(array)), target);
    }

    static int count_dp(int[] array, int target, int[] dp) {
        if (target < 0) {
            return 0;
        }
        if (target == 0) {
            return 1;
        }
        if (dp[target] > (0 - 1)) {
            return dp[target];
        }
        int total_1 = 0;
        int i_2 = 0;
        while (i_2 < array.length) {
            total_1 = total_1 + count_dp(((int[])(array)), target - array[i_2], ((int[])(dp)));
            i_2 = i_2 + 1;
        }
dp[target] = total_1;
        return total_1;
    }

    static int combination_sum_iv_dp_array(int[] array, int target) {
        int[] dp = ((int[])(make_list(target + 1, -1)));
        return count_dp(((int[])(array)), target, ((int[])(dp)));
    }

    static int combination_sum_iv_bottom_up(int n, int[] array, int target) {
        int[] dp_1 = ((int[])(make_list(target + 1, 0)));
dp_1[0] = 1;
        int i_3 = 1;
        while (i_3 <= target) {
            int j = 0;
            while (j < n) {
                if (i_3 - array[j] >= 0) {
dp_1[i_3] = dp_1[i_3] + dp_1[i_3 - array[j]];
                }
                j = j + 1;
            }
            i_3 = i_3 + 1;
        }
        return dp_1[target];
    }
    public static void main(String[] args) {
        System.out.println(_p(combination_sum_iv(((int[])(new int[]{1, 2, 5})), 5)));
        System.out.println(_p(combination_sum_iv_dp_array(((int[])(new int[]{1, 2, 5})), 5)));
        System.out.println(_p(combination_sum_iv_bottom_up(3, ((int[])(new int[]{1, 2, 5})), 5)));
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
