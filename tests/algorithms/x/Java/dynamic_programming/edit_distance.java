public class Main {

    static int min3(int a, int b, int c) {
        int m = a;
        if (b < m) {
            m = b;
        }
        if (c < m) {
            m = c;
        }
        return m;
    }

    static int helper_top_down(String word1, String word2, int[][] dp, int i, int j) {
        if (i < 0) {
            return j + 1;
        }
        if (j < 0) {
            return i + 1;
        }
        if (dp[i][j] != (0 - 1)) {
            return dp[i][j];
        }
        if ((_substr(word1, i, i + 1).equals(_substr(word2, j, j + 1)))) {
dp[i][j] = helper_top_down(word1, word2, ((int[][])(dp)), i - 1, j - 1);
        } else {
            int insert = helper_top_down(word1, word2, ((int[][])(dp)), i, j - 1);
            int delete = helper_top_down(word1, word2, ((int[][])(dp)), i - 1, j);
            int replace = helper_top_down(word1, word2, ((int[][])(dp)), i - 1, j - 1);
dp[i][j] = 1 + min3(insert, delete, replace);
        }
        return dp[i][j];
    }

    static int min_dist_top_down(String word1, String word2) {
        int m_1 = _runeLen(word1);
        int n = _runeLen(word2);
        int[][] dp = ((int[][])(new int[][]{}));
        for (int _v = 0; _v < m_1; _v++) {
            int[] row = ((int[])(new int[]{}));
            for (int _2 = 0; _2 < n; _2++) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0 - 1)).toArray()));
            }
            dp = ((int[][])(appendObj((int[][])dp, row)));
        }
        return helper_top_down(word1, word2, ((int[][])(dp)), m_1 - 1, n - 1);
    }

    static int min_dist_bottom_up(String word1, String word2) {
        int m_2 = _runeLen(word1);
        int n_1 = _runeLen(word2);
        int[][] dp_1 = ((int[][])(new int[][]{}));
        for (int _v = 0; _v < (m_2 + 1); _v++) {
            int[] row_1 = ((int[])(new int[]{}));
            for (int _2 = 0; _2 < (n_1 + 1); _2++) {
                row_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_1), java.util.stream.IntStream.of(0)).toArray()));
            }
            dp_1 = ((int[][])(appendObj((int[][])dp_1, row_1)));
        }
        for (int i = 0; i < (m_2 + 1); i++) {
            for (int j = 0; j < (n_1 + 1); j++) {
                if (i == 0) {
dp_1[i][j] = j;
                } else                 if (j == 0) {
dp_1[i][j] = i;
                } else                 if ((_substr(word1, i - 1, i).equals(_substr(word2, j - 1, j)))) {
dp_1[i][j] = dp_1[i - 1][j - 1];
                } else {
                    int insert_1 = dp_1[i][j - 1];
                    int delete_1 = dp_1[i - 1][j];
                    int replace_1 = dp_1[i - 1][j - 1];
dp_1[i][j] = 1 + min3(insert_1, delete_1, replace_1);
                }
            }
        }
        return dp_1[m_2][n_1];
    }
    public static void main(String[] args) {
        System.out.println(_p(min_dist_top_down("intention", "execution")));
        System.out.println(_p(min_dist_top_down("intention", "")));
        System.out.println(_p(min_dist_top_down("", "")));
        System.out.println(_p(min_dist_bottom_up("intention", "execution")));
        System.out.println(_p(min_dist_bottom_up("intention", "")));
        System.out.println(_p(min_dist_bottom_up("", "")));
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
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
