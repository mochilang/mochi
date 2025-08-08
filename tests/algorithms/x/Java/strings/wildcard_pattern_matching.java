public class Main {

    static boolean[][] make_matrix_bool(int rows, int cols, boolean init) {
        boolean[][] matrix = ((boolean[][])(new boolean[][]{}));
        for (int _v = 0; _v < rows; _v++) {
            boolean[] row = ((boolean[])(new boolean[]{}));
            for (int _2 = 0; _2 < cols; _2++) {
                row = ((boolean[])(appendBool(row, ((Boolean)(init)))));
            }
            matrix = ((boolean[][])(appendObj(matrix, row)));
        }
        return matrix;
    }

    static boolean match_pattern(String input_string, String pattern) {
        int len_string = _runeLen(input_string) + 1;
        int len_pattern = _runeLen(pattern) + 1;
        boolean[][] dp = ((boolean[][])(make_matrix_bool(len_string, len_pattern, false)));
        boolean[] row0 = ((boolean[])(dp[0]));
row0[0] = true;
dp[0] = ((boolean[])(row0));
        int j = 1;
        while (j < len_pattern) {
            row0 = ((boolean[])(dp[0]));
            if ((_substr(pattern, j - 1, j).equals("*"))) {
row0[j] = row0[j - 2];
            } else {
row0[j] = false;
            }
dp[0] = ((boolean[])(row0));
            j = j + 1;
        }
        int i = 1;
        while (i < len_string) {
            boolean[] row_1 = ((boolean[])(dp[i]));
            int j2 = 1;
            while (j2 < len_pattern) {
                String s_char = _substr(input_string, i - 1, i);
                String p_char = _substr(pattern, j2 - 1, j2);
                if ((s_char.equals(p_char)) || (p_char.equals("."))) {
row_1[j2] = dp[i - 1][j2 - 1];
                } else                 if ((p_char.equals("*"))) {
                    boolean val = dp[i][j2 - 2];
                    String prev_p = _substr(pattern, j2 - 2, j2 - 1);
                    if (!val && ((prev_p.equals(s_char)) || (prev_p.equals(".")))) {
                        val = dp[i - 1][j2];
                    }
row_1[j2] = val;
                } else {
row_1[j2] = false;
                }
                j2 = j2 + 1;
            }
dp[i] = ((boolean[])(row_1));
            i = i + 1;
        }
        return dp[len_string - 1][len_pattern - 1];
    }

    static void main() {
        if (!(Boolean)match_pattern("aab", "c*a*b")) {
            throw new RuntimeException(String.valueOf("case1 failed"));
        }
        if (((Boolean)(match_pattern("dabc", "*abc")))) {
            throw new RuntimeException(String.valueOf("case2 failed"));
        }
        if (((Boolean)(match_pattern("aaa", "aa")))) {
            throw new RuntimeException(String.valueOf("case3 failed"));
        }
        if (!(Boolean)match_pattern("aaa", "a.a")) {
            throw new RuntimeException(String.valueOf("case4 failed"));
        }
        if (((Boolean)(match_pattern("aaab", "aa*")))) {
            throw new RuntimeException(String.valueOf("case5 failed"));
        }
        if (!(Boolean)match_pattern("aaab", ".*")) {
            throw new RuntimeException(String.valueOf("case6 failed"));
        }
        if (((Boolean)(match_pattern("a", "bbbb")))) {
            throw new RuntimeException(String.valueOf("case7 failed"));
        }
        if (((Boolean)(match_pattern("", "bbbb")))) {
            throw new RuntimeException(String.valueOf("case8 failed"));
        }
        if (((Boolean)(match_pattern("a", "")))) {
            throw new RuntimeException(String.valueOf("case9 failed"));
        }
        if (!(Boolean)match_pattern("", "")) {
            throw new RuntimeException(String.valueOf("case10 failed"));
        }
        System.out.println(_p(match_pattern("aab", "c*a*b")));
        System.out.println(_p(match_pattern("dabc", "*abc")));
        System.out.println(_p(match_pattern("aaa", "aa")));
        System.out.println(_p(match_pattern("aaa", "a.a")));
        System.out.println(_p(match_pattern("aaab", "aa*")));
        System.out.println(_p(match_pattern("aaab", ".*")));
        System.out.println(_p(match_pattern("a", "bbbb")));
        System.out.println(_p(match_pattern("", "bbbb")));
        System.out.println(_p(match_pattern("a", "")));
        System.out.println(_p(match_pattern("", "")));
    }
    public static void main(String[] args) {
        main();
    }

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
