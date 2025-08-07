public class Main {

    static boolean is_substring_divisible(int[] num) {
        if (Math.floorMod(num[3], 2) != 0) {
            return false;
        }
        if (Math.floorMod((num[2] + num[3] + num[4]), 3) != 0) {
            return false;
        }
        if (Math.floorMod(num[5], 5) != 0) {
            return false;
        }
        int[] primes = ((int[])(new int[]{7, 11, 13, 17}));
        int i = 0;
        while (i < primes.length) {
            int p = primes[i];
            int idx = i + 4;
            int val = num[idx] * 100 + num[idx + 1] * 10 + num[idx + 2];
            if (Math.floorMod(val, p) != 0) {
                return false;
            }
            i = i + 1;
        }
        return true;
    }

    static int[] remove_at(int[] xs, int idx) {
        int[] res = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < xs.length) {
            if (i_1 != idx) {
                res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(xs[i_1])).toArray()));
            }
            i_1 = i_1 + 1;
        }
        return res;
    }

    static int digits_to_number(int[] xs) {
        int value = 0;
        int i_2 = 0;
        while (i_2 < xs.length) {
            value = value * 10 + xs[i_2];
            i_2 = i_2 + 1;
        }
        return value;
    }

    static int search(int[] prefix, int[] remaining) {
        if (remaining.length == 0) {
            if (((Boolean)(is_substring_divisible(((int[])(prefix)))))) {
                return digits_to_number(((int[])(prefix)));
            }
            return 0;
        }
        int total = 0;
        int i_3 = 0;
        while (i_3 < remaining.length) {
            int d = remaining[i_3];
            int[] next_prefix = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(prefix), java.util.stream.IntStream.of(d)).toArray()));
            int[] next_remaining = ((int[])(remove_at(((int[])(remaining)), i_3)));
            total = total + search(((int[])(next_prefix)), ((int[])(next_remaining)));
            i_3 = i_3 + 1;
        }
        return total;
    }

    static int solution(int n) {
        int[] digits = ((int[])(new int[]{}));
        int i_4 = 0;
        while (i_4 < n) {
            digits = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(digits), java.util.stream.IntStream.of(i_4)).toArray()));
            i_4 = i_4 + 1;
        }
        return search(((int[])(new int[]{})), ((int[])(digits)));
    }
    public static void main(String[] args) {
        System.out.println("solution() =" + " " + String.valueOf(solution(10)));
    }
}
