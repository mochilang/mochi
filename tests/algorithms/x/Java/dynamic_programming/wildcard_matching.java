public class Main {

    static boolean[] make_bool_list(int n) {
        boolean[] row = ((boolean[])(new boolean[]{}));
        int i = 0;
        while (i < n) {
            row = ((boolean[])(appendBool(row, false)));
            i = i + 1;
        }
        return row;
    }

    static boolean[][] make_bool_matrix(int rows, int cols) {
        boolean[][] matrix = ((boolean[][])(new boolean[][]{}));
        int i_1 = 0;
        while (i_1 < rows) {
            matrix = ((boolean[][])(appendObj(matrix, make_bool_list(cols))));
            i_1 = i_1 + 1;
        }
        return matrix;
    }

    static boolean is_match(String s, String p) {
        int n = _runeLen(s);
        int m = _runeLen(p);
        boolean[][] dp = ((boolean[][])(make_bool_matrix(n + 1, m + 1)));
dp[0][0] = true;
        int j = 1;
        while (j <= m) {
            if ((p.substring(j - 1, j).equals("*"))) {
dp[0][j] = dp[0][j - 1];
            }
            j = j + 1;
        }
        int i_2 = 1;
        while (i_2 <= n) {
            int j2 = 1;
            while (j2 <= m) {
                String pc = p.substring(j2 - 1, j2);
                String sc = s.substring(i_2 - 1, i_2);
                if ((pc.equals(sc)) || (pc.equals("?"))) {
dp[i_2][j2] = dp[i_2 - 1][j2 - 1];
                } else                 if ((pc.equals("*"))) {
                    if (dp[i_2 - 1][j2] || dp[i_2][j2 - 1]) {
dp[i_2][j2] = true;
                    }
                }
                j2 = j2 + 1;
            }
            i_2 = i_2 + 1;
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
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            print_bool(is_match("abc", "a*c"));
            print_bool(is_match("abc", "a*d"));
            print_bool(is_match("baaabab", "*****ba*****ab"));
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
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
