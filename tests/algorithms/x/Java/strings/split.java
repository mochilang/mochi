public class Main {

    static String[] split_with_sep(String s, String sep) {
        String[] parts = ((String[])(new String[]{}));
        int last = 0;
        int i = 0;
        while (i < _runeLen(s)) {
            String ch = _substr(s, i, i + 1);
            if ((ch.equals(sep))) {
                parts = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(_substr(s, last, i))).toArray(String[]::new)));
                last = i + 1;
            }
            if (i + 1 == _runeLen(s)) {
                parts = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(_substr(s, last, i + 1))).toArray(String[]::new)));
            }
            i = i + 1;
        }
        return parts;
    }

    static String[] split(String s) {
        return split_with_sep(s, " ");
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(split_with_sep("apple#banana#cherry#orange", "#")));
            System.out.println(_p(split("Hello there")));
            System.out.println(_p(split_with_sep("11/22/63", "/")));
            System.out.println(_p(split_with_sep("12:43:39", ":")));
            System.out.println(_p(split_with_sep(";abbb;;c;", ";")));
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
