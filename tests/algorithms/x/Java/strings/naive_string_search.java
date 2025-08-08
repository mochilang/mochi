public class Main {

    static int[] naive_string_search(String text, String pattern) {
        int pat_len = _runeLen(pattern);
        int[] positions = ((int[])(new int[]{}));
        int i = 0;
        while (i <= _runeLen(text) - pat_len) {
            boolean match_found = true;
            int j = 0;
            while (j < pat_len) {
                if (!(text.substring(i + j, i + j+1).equals(pattern.substring(j, j+1)))) {
                    match_found = false;
                    break;
                }
                j = j + 1;
            }
            if (match_found) {
                positions = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(positions), java.util.stream.IntStream.of(i)).toArray()));
            }
            i = i + 1;
        }
        return positions;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(naive_string_search("ABAAABCDBBABCDDEBCABC", "ABC"));
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
}
