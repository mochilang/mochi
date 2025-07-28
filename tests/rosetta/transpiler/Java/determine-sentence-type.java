public class Main {

    static int indexOf(String s, String ch) {
        int i = 0;
        while (i < _runeLen(s)) {
            if ((_substr(s, i, i + 1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static String join(String[] xs, String sep) {
        String res = "";
        int i = 0;
        while (i < xs.length) {
            if (i > 0) {
                res = res + sep;
            }
            res = res + xs[i];
            i = i + 1;
        }
        return res;
    }

    static String sentenceType(String s) {
        if (_runeLen(s) == 0) {
            return "";
        }
        String[] types = new String[]{};
        int i = 0;
        while (i < _runeLen(s)) {
            String ch = _substr(s, i, i + 1);
            if ((ch.equals("?"))) {
                types = java.util.stream.Stream.concat(java.util.Arrays.stream(types), java.util.stream.Stream.of("Q")).toArray(String[]::new);
            } else             if ((ch.equals("!"))) {
                types = java.util.stream.Stream.concat(java.util.Arrays.stream(types), java.util.stream.Stream.of("E")).toArray(String[]::new);
            } else             if ((ch.equals("."))) {
                types = java.util.stream.Stream.concat(java.util.Arrays.stream(types), java.util.stream.Stream.of("S")).toArray(String[]::new);
            }
            i = i + 1;
        }
        String last = _substr(s, _runeLen(s) - 1, _runeLen(s));
        if (((Number)("?!.".indexOf(last))).intValue() == (-1)) {
            types = java.util.stream.Stream.concat(java.util.Arrays.stream(types), java.util.stream.Stream.of("N")).toArray(String[]::new);
        }
        return join(types, "|");
    }

    static void main() {
        String s = "hi there, how are you today? I'd like to present to you the washing machine 9001. You have been nominated to win one of these! Just make sure you don't break it";
        String result = String.valueOf(sentenceType(s));
        System.out.println(result);
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

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
