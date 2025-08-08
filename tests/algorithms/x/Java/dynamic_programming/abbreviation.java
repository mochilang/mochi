public class Main {

    static int index_of(String s, String ch) {
        int i = 0;
        while (i < _runeLen(s)) {
            if ((s.substring(i, i+1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static int ord(String ch) {
        String upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String lower = "abcdefghijklmnopqrstuvwxyz";
        int idx = index_of(upper, ch);
        if (idx >= 0) {
            return 65 + idx;
        }
        idx = index_of(lower, ch);
        if (idx >= 0) {
            return 97 + idx;
        }
        return 0;
    }

    static String chr(int n) {
        String upper_1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String lower_1 = "abcdefghijklmnopqrstuvwxyz";
        if (n >= 65 && n < 91) {
            return upper_1.substring(n - 65, n - 64);
        }
        if (n >= 97 && n < 123) {
            return lower_1.substring(n - 97, n - 96);
        }
        return "?";
    }

    static String to_upper_char(String c) {
        int code = ord(c);
        if (code >= 97 && code <= 122) {
            return chr(code - 32);
        }
        return c;
    }

    static boolean is_lower(String c) {
        int code_1 = ord(c);
        return code_1 >= 97 && code_1 <= 122;
    }

    static boolean abbr(String a, String b) {
        int n = _runeLen(a);
        int m = _runeLen(b);
        boolean[][] dp = ((boolean[][])(new boolean[][]{}));
        int i_1 = 0;
        while (i_1 <= n) {
            boolean[] row = ((boolean[])(new boolean[]{}));
            int j = 0;
            while (j <= m) {
                row = ((boolean[])(appendBool(row, false)));
                j = j + 1;
            }
            dp = ((boolean[][])(appendObj((boolean[][])dp, row)));
            i_1 = i_1 + 1;
        }
dp[0][0] = true;
        i_1 = 0;
        while (i_1 < n) {
            int j_1 = 0;
            while (j_1 <= m) {
                if (((Boolean)(dp[i_1][j_1]))) {
                    if (j_1 < m && (to_upper_char(a.substring(i_1, i_1+1)).equals(b.substring(j_1, j_1+1)))) {
dp[i_1 + 1][j_1 + 1] = true;
                    }
                    if (((Boolean)(is_lower(a.substring(i_1, i_1+1))))) {
dp[i_1 + 1][j_1] = true;
                    }
                }
                j_1 = j_1 + 1;
            }
            i_1 = i_1 + 1;
        }
        return dp[n][m];
    }

    static void print_bool(boolean b) {
        if (((Boolean)(b))) {
            System.out.println(true ? "True" : "False");
        } else {
            System.out.println(false ? "True" : "False");
        }
    }
    public static void main(String[] args) {
        print_bool(abbr("daBcd", "ABC"));
        print_bool(abbr("dBcd", "ABC"));
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
}
