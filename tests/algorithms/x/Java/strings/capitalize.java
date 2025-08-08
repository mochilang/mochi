public class Main {
    static String lowercase;
    static String uppercase;

    static int index_of(String s, String c) {
        int i = 0;
        while (i < _runeLen(s)) {
            if ((_substr(s, i, i + 1).equals(c))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static String capitalize(String sentence) {
        if (_runeLen(sentence) == 0) {
            return "";
        }
        String first = _substr(sentence, 0, 1);
        int idx = index_of(lowercase, first);
        String capital = String.valueOf(idx >= 0 ? _substr(uppercase, idx, idx + 1) : first);
        return capital + _substr(sentence, 1, _runeLen(sentence));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            lowercase = "abcdefghijklmnopqrstuvwxyz";
            uppercase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
            System.out.println(capitalize("hello world"));
            System.out.println(capitalize("123 hello world"));
            System.out.println(capitalize(" hello world"));
            System.out.println(capitalize("a"));
            System.out.println(capitalize(""));
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
