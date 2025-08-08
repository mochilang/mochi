public class Main {
    static String text;
    static String[] keywords;

    static java.util.Map<String,int[]> search_all(String text, String[] keywords) {
        java.util.Map<String,int[]> result = ((java.util.Map<String,int[]>)(new java.util.LinkedHashMap<String, int[]>()));
        for (String word : keywords) {
            int[] positions = ((int[])(new int[]{}));
            int m = _runeLen(word);
            int i = 0;
            while (i <= _runeLen(text) - m) {
                if ((_substr(text, i, i + m).equals(word))) {
                    positions = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(positions), java.util.stream.IntStream.of(i)).toArray()));
                }
                i = i + 1;
            }
            if (positions.length > 0) {
result.put(word, ((int[])(positions)));
            }
        }
        return result;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            text = "whatever, err ... , wherever";
            keywords = ((String[])(new String[]{"what", "hat", "ver", "er"}));
            System.out.println(search_all(text, ((String[])(keywords))));
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
