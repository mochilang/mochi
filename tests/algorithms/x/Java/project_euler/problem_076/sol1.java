public class Main {

    static int solution(int m) {
        int[][] memo = ((int[][])(new int[][]{}));
        int i = 0;
        while (i <= m) {
            int[] row = ((int[])(new int[]{}));
            int j = 0;
            while (j < m) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray()));
                j = j + 1;
            }
            memo = ((int[][])(appendObj(memo, row)));
            i = i + 1;
        }
        i = 0;
        while (i <= m) {
memo[i][0] = 1;
            i = i + 1;
        }
        int n = 0;
        while (n <= m) {
            int k = 1;
            while (k < m) {
memo[n][k] = memo[n][k] + memo[n][k - 1];
                if (n > k) {
memo[n][k] = memo[n][k] + memo[n - k - 1][k];
                }
                k = k + 1;
            }
            n = n + 1;
        }
        return memo[m][m - 1] - 1;
    }
    public static void main(String[] args) {
        System.out.println(_p(solution(100)));
        System.out.println(_p(solution(50)));
        System.out.println(_p(solution(30)));
        System.out.println(_p(solution(10)));
        System.out.println(_p(solution(5)));
        System.out.println(_p(solution(3)));
        System.out.println(_p(solution(2)));
        System.out.println(_p(solution(1)));
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
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
