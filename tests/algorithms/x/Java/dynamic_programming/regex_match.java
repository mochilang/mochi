public class Main {

    static boolean recursive_match(String text, String pattern) {
        if (_runeLen(pattern) == 0) {
            return _runeLen(text) == 0;
        }
        if (_runeLen(text) == 0) {
            if (_runeLen(pattern) >= 2 && (_substr(pattern, _runeLen(pattern) - 1, _runeLen(pattern)).equals("*"))) {
                return recursive_match(text, _substr(pattern, 0, _runeLen(pattern) - 2));
            }
            return false;
        }
        String last_text = _substr(text, _runeLen(text) - 1, _runeLen(text));
        String last_pattern = _substr(pattern, _runeLen(pattern) - 1, _runeLen(pattern));
        if ((last_text.equals(last_pattern)) || (last_pattern.equals("."))) {
            return recursive_match(_substr(text, 0, _runeLen(text) - 1), _substr(pattern, 0, _runeLen(pattern) - 1));
        }
        if ((last_pattern.equals("*"))) {
            if (((Boolean)(recursive_match(_substr(text, 0, _runeLen(text) - 1), pattern)))) {
                return true;
            }
            return recursive_match(text, _substr(pattern, 0, _runeLen(pattern) - 2));
        }
        return false;
    }

    static boolean dp_match(String text, String pattern) {
        int m = _runeLen(text);
        int n = _runeLen(pattern);
        boolean[][] dp = ((boolean[][])(new boolean[][]{}));
        int i = 0;
        while (i <= m) {
            boolean[] row = ((boolean[])(new boolean[]{}));
            int j = 0;
            while (j <= n) {
                row = ((boolean[])(appendBool(row, false)));
                j = j + 1;
            }
            dp = ((boolean[][])(appendObj(dp, row)));
            i = i + 1;
        }
dp[0][0] = true;
        int j_1 = 1;
        while (j_1 <= n) {
            if ((_substr(pattern, j_1 - 1, j_1).equals("*")) && j_1 >= 2) {
                if (((Boolean)(dp[0][j_1 - 2]))) {
dp[0][j_1] = true;
                }
            }
            j_1 = j_1 + 1;
        }
        i = 1;
        while (i <= m) {
            j_1 = 1;
            while (j_1 <= n) {
                String p_char = _substr(pattern, j_1 - 1, j_1);
                String t_char = _substr(text, i - 1, i);
                if ((p_char.equals(".")) || (p_char.equals(t_char))) {
                    if (((Boolean)(dp[i - 1][j_1 - 1]))) {
dp[i][j_1] = true;
                    }
                } else                 if ((p_char.equals("*"))) {
                    if (j_1 >= 2) {
                        if (((Boolean)(dp[i][j_1 - 2]))) {
dp[i][j_1] = true;
                        }
                        String prev_p = _substr(pattern, j_1 - 2, j_1 - 1);
                        if ((prev_p.equals(".")) || (prev_p.equals(t_char))) {
                            if (((Boolean)(dp[i - 1][j_1]))) {
dp[i][j_1] = true;
                            }
                        }
                    }
                } else {
dp[i][j_1] = false;
                }
                j_1 = j_1 + 1;
            }
            i = i + 1;
        }
        return dp[m][n];
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
            print_bool(recursive_match("abc", "a.c"));
            print_bool(recursive_match("abc", "af*.c"));
            print_bool(recursive_match("abc", "a.c*"));
            print_bool(recursive_match("abc", "a.c*d"));
            print_bool(recursive_match("aa", ".*"));
            print_bool(dp_match("abc", "a.c"));
            print_bool(dp_match("abc", "af*.c"));
            print_bool(dp_match("abc", "a.c*"));
            print_bool(dp_match("abc", "a.c*d"));
            print_bool(dp_match("aa", ".*"));
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

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
