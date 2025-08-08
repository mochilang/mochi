public class Main {

    static int partition(int m) {
        int[][] memo = ((int[][])(new int[][]{}));
        int i = 0;
        while (i < m + 1) {
            int[] row = ((int[])(new int[]{}));
            int j = 0;
            while (j < m) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray()));
                j = j + 1;
            }
            memo = ((int[][])(appendObj((int[][])memo, row)));
            i = i + 1;
        }
        i = 0;
        while (i < m + 1) {
memo[i][0] = 1;
            i = i + 1;
        }
        int n = 0;
        while (n < m + 1) {
            int k = 1;
            while (k < m) {
memo[n][k] = memo[n][k] + memo[n][k - 1];
                if (n - k > 0) {
memo[n][k] = memo[n][k] + memo[n - k - 1][k];
                }
                k = k + 1;
            }
            n = n + 1;
        }
        return memo[m][m - 1];
    }
    public static void main(String[] args) {
        System.out.println(partition(5));
        System.out.println(partition(7));
        System.out.println(partition(100));
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
