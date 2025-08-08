public class Main {

    static int[] prefix_function(String s) {
        int[] pi = ((int[])(new int[]{}));
        int i = 0;
        while (i < _runeLen(s)) {
            pi = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(pi), java.util.stream.IntStream.of(0)).toArray()));
            i = i + 1;
        }
        i = 1;
        while (i < _runeLen(s)) {
            int j = pi[i - 1];
            while (j > 0 && !(s.substring(i, i+1).equals(s.substring(j, j+1)))) {
                j = pi[j - 1];
            }
            if ((s.substring(i, i+1).equals(s.substring(j, j+1)))) {
                j = j + 1;
            }
pi[i] = j;
            i = i + 1;
        }
        return pi;
    }

    static int longest_prefix(String s) {
        int[] pi_1 = ((int[])(prefix_function(s)));
        int max_val = 0;
        int i_1 = 0;
        while (i_1 < pi_1.length) {
            if (pi_1[i_1] > max_val) {
                max_val = pi_1[i_1];
            }
            i_1 = i_1 + 1;
        }
        return max_val;
    }

    static boolean list_eq_int(int[] a, int[] b) {
        if (a.length != b.length) {
            return false;
        }
        int i_2 = 0;
        while (i_2 < a.length) {
            if (a[i_2] != b[i_2]) {
                return false;
            }
            i_2 = i_2 + 1;
        }
        return true;
    }

    static void test_prefix_function() {
        String s1 = "aabcdaabc";
        int[] expected1 = ((int[])(new int[]{0, 1, 0, 0, 0, 1, 2, 3, 4}));
        int[] r1 = ((int[])(prefix_function(s1)));
        if (!(Boolean)list_eq_int(((int[])(r1)), ((int[])(expected1)))) {
            throw new RuntimeException(String.valueOf("prefix_function aabcdaabc failed"));
        }
        String s2 = "asdasdad";
        int[] expected2 = ((int[])(new int[]{0, 0, 0, 1, 2, 3, 4, 0}));
        int[] r2 = ((int[])(prefix_function(s2)));
        if (!(Boolean)list_eq_int(((int[])(r2)), ((int[])(expected2)))) {
            throw new RuntimeException(String.valueOf("prefix_function asdasdad failed"));
        }
    }

    static void test_longest_prefix() {
        if (longest_prefix("aabcdaabc") != 4) {
            throw new RuntimeException(String.valueOf("longest_prefix example1 failed"));
        }
        if (longest_prefix("asdasdad") != 4) {
            throw new RuntimeException(String.valueOf("longest_prefix example2 failed"));
        }
        if (longest_prefix("abcab") != 2) {
            throw new RuntimeException(String.valueOf("longest_prefix example3 failed"));
        }
    }

    static void main() {
        test_prefix_function();
        test_longest_prefix();
        int[] r1_1 = ((int[])(prefix_function("aabcdaabc")));
        int[] r2_1 = ((int[])(prefix_function("asdasdad")));
        System.out.println(_p(r1_1));
        System.out.println(_p(r2_1));
        System.out.println(_p(longest_prefix("aabcdaabc")));
        System.out.println(_p(longest_prefix("abcab")));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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
