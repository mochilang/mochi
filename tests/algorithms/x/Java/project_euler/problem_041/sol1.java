public class Main {

    static boolean is_prime(int number) {
        if ((1 < number) && (number < 4)) {
            return true;
        } else         if ((number < 2) || (Math.floorMod(number, 2) == 0) || (Math.floorMod(number, 3) == 0)) {
            return false;
        }
        int i = 5;
        while (i * i <= number) {
            if ((Math.floorMod(number, i) == 0) || (Math.floorMod(number, (i + 2)) == 0)) {
                return false;
            }
            i = i + 6;
        }
        return true;
    }

    static int[] remove_at(int[] xs, int index) {
        int[] res = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < xs.length) {
            if (i_1 != index) {
                res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(xs[i_1])).toArray()));
            }
            i_1 = i_1 + 1;
        }
        return res;
    }

    static int[] collect_primes(int[] nums, int current, int[] primes) {
        if (nums.length == 0) {
            if (((Boolean)(is_prime(current)))) {
                primes = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(primes), java.util.stream.IntStream.of(current)).toArray()));
            }
            return primes;
        }
        int i_2 = 0;
        int[] res_1 = ((int[])(primes));
        while (i_2 < nums.length) {
            int digit = nums[i_2];
            int[] remaining = ((int[])(remove_at(((int[])(nums)), i_2)));
            res_1 = ((int[])(collect_primes(((int[])(remaining)), current * 10 + digit, ((int[])(res_1)))));
            i_2 = i_2 + 1;
        }
        return res_1;
    }

    static int max_list(int[] nums) {
        int m = 0;
        int i_3 = 0;
        while (i_3 < nums.length) {
            if (nums[i_3] > m) {
                m = nums[i_3];
            }
            i_3 = i_3 + 1;
        }
        return m;
    }

    static int solution(int n) {
        int[] digits = ((int[])(new int[]{}));
        int i_4 = 1;
        while (i_4 <= n) {
            digits = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(digits), java.util.stream.IntStream.of(i_4)).toArray()));
            i_4 = i_4 + 1;
        }
        int[] primes = ((int[])(collect_primes(((int[])(digits)), 0, ((int[])(new int[]{})))));
        if (primes.length == 0) {
            return 0;
        }
        return max_list(((int[])(primes)));
    }
    public static void main(String[] args) {
        System.out.println("solution() = " + _p(solution(7)));
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
