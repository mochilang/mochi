public class Main {

    static int int_sqrt(int n) {
        int x = 1;
        while ((x + 1) * (x + 1) <= n) {
            x = x + 1;
        }
        return x;
    }

    static int solution(int limit) {
        int[] sum_divs = ((int[])(new int[]{}));
        int i = 0;
        while (i <= limit) {
            sum_divs = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(sum_divs), java.util.stream.IntStream.of(1)).toArray()));
            i = i + 1;
        }
        int sqrt_limit = int_sqrt(limit);
        i = 2;
        while (i <= sqrt_limit) {
sum_divs[i * i] = sum_divs[i * i] + i;
            int k = i + 1;
            while (k <= Math.floorDiv(limit, i)) {
sum_divs[k * i] = sum_divs[k * i] + k + i;
                k = k + 1;
            }
            i = i + 1;
        }
        boolean[] is_abundant = ((boolean[])(new boolean[]{}));
        i = 0;
        while (i <= limit) {
            is_abundant = ((boolean[])(appendBool(is_abundant, false)));
            i = i + 1;
        }
        int[] abundants = ((int[])(new int[]{}));
        int res = 0;
        int n = 1;
        while (n <= limit) {
            if (sum_divs[n] > n) {
                abundants = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(abundants), java.util.stream.IntStream.of(n)).toArray()));
is_abundant[n] = true;
            }
            boolean has_pair = false;
            int j = 0;
            while (j < abundants.length) {
                int a = abundants[j];
                if (a > n) {
                    break;
                }
                int b = n - a;
                if (b <= limit && ((Boolean)(is_abundant[b]))) {
                    has_pair = true;
                    break;
                }
                j = j + 1;
            }
            if (!has_pair) {
                res = res + n;
            }
            n = n + 1;
        }
        return res;
    }
    public static void main(String[] args) {
        System.out.println(_p(solution(28123)));
    }

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
