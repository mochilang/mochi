public class Main {
    static class LcsResult {
        int length;
        String sequence;
        LcsResult(int length, String sequence) {
            this.length = length;
            this.sequence = sequence;
        }
        LcsResult() {}
        @Override public String toString() {
            return String.format("{'length': %s, 'sequence': '%s'}", String.valueOf(length), String.valueOf(sequence));
        }
    }

    static String a;
    static String b;
    static LcsResult res;

    static int[][] zeros_matrix(int rows, int cols) {
        int[][] matrix = ((int[][])(new int[][]{}));
        int i = 0;
        while (i <= rows) {
            int[] row = ((int[])(new int[]{}));
            int j = 0;
            while (j <= cols) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray()));
                j = j + 1;
            }
            matrix = ((int[][])(appendObj((int[][])matrix, row)));
            i = i + 1;
        }
        return matrix;
    }

    static LcsResult longest_common_subsequence(String x, String y) {
        int m = _runeLen(x);
        int n = _runeLen(y);
        int[][] dp = ((int[][])(zeros_matrix(m, n)));
        int i_1 = 1;
        while (i_1 <= m) {
            int j_1 = 1;
            while (j_1 <= n) {
                if ((x.substring(i_1 - 1, i_1 - 1+1).equals(y.substring(j_1 - 1, j_1 - 1+1)))) {
dp[i_1][j_1] = dp[i_1 - 1][j_1 - 1] + 1;
                } else                 if (dp[i_1 - 1][j_1] > dp[i_1][j_1 - 1]) {
dp[i_1][j_1] = dp[i_1 - 1][j_1];
                } else {
dp[i_1][j_1] = dp[i_1][j_1 - 1];
                }
                j_1 = j_1 + 1;
            }
            i_1 = i_1 + 1;
        }
        String seq = "";
        int i2 = m;
        int j2 = n;
        while (i2 > 0 && j2 > 0) {
            if ((x.substring(i2 - 1, i2 - 1+1).equals(y.substring(j2 - 1, j2 - 1+1)))) {
                seq = x.substring(i2 - 1, i2 - 1+1) + seq;
                i2 = i2 - 1;
                j2 = j2 - 1;
            } else             if (dp[i2 - 1][j2] >= dp[i2][j2 - 1]) {
                i2 = i2 - 1;
            } else {
                j2 = j2 - 1;
            }
        }
        return new LcsResult(dp[m][n], seq);
    }
    public static void main(String[] args) {
        a = "AGGTAB";
        b = "GXTXAYB";
        res = longest_common_subsequence(a, b);
        System.out.println("len = " + _p(res.length) + ", sub-sequence = " + res.sequence);
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
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
