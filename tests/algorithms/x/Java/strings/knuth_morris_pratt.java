public class Main {
    static String text;
    static String pattern;

    static int[] get_failure_array(String pattern) {
        int[] failure = ((int[])(new int[]{0}));
        int i = 0;
        int j = 1;
        while (j < _runeLen(pattern)) {
            if ((_substr(pattern, i, i + 1).equals(_substr(pattern, j, j + 1)))) {
                i = i + 1;
            } else             if (i > 0) {
                i = failure[i - 1];
                continue;
            }
            j = j + 1;
            failure = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(failure), java.util.stream.IntStream.of(i)).toArray()));
        }
        return failure;
    }

    static int knuth_morris_pratt(String text, String pattern) {
        int[] failure_1 = ((int[])(get_failure_array(pattern)));
        int i_1 = 0;
        int j_1 = 0;
        while (i_1 < _runeLen(text)) {
            if ((_substr(pattern, j_1, j_1 + 1).equals(_substr(text, i_1, i_1 + 1)))) {
                if (j_1 == _runeLen(pattern) - 1) {
                    return i_1 - j_1;
                }
                j_1 = j_1 + 1;
            } else             if (j_1 > 0) {
                j_1 = failure_1[j_1 - 1];
                continue;
            }
            i_1 = i_1 + 1;
        }
        return -1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            text = "abcxabcdabxabcdabcdabcy";
            pattern = "abcdabcy";
            System.out.println(knuth_morris_pratt(text, pattern));
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

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
