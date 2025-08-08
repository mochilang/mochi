public class Main {

    static boolean contains(String chars, String ch) {
        int i = 0;
        while (i < _runeLen(chars)) {
            if ((chars.substring(i, i+1).equals(ch))) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static String substring(String s, int start, int end) {
        String res = "";
        int i_1 = start;
        while (i_1 < end) {
            res = res + s.substring(i_1, i_1+1);
            i_1 = i_1 + 1;
        }
        return res;
    }

    static String strip_chars(String user_string, String characters) {
        int start = 0;
        int end = _runeLen(user_string);
        while (start < end && ((Boolean)(contains(characters, user_string.substring(start, start+1))))) {
            start = start + 1;
        }
        while (end > start && ((Boolean)(contains(characters, user_string.substring(end - 1, end - 1+1))))) {
            end = end - 1;
        }
        return _substr(user_string, start, end);
    }

    static String strip(String user_string) {
        return strip_chars(user_string, " \t\n\r");
    }

    static void test_strip() {
        if (!(strip("   hello   ").equals("hello"))) {
            throw new RuntimeException(String.valueOf("test1 failed"));
        }
        if (!(strip_chars("...world...", ".").equals("world"))) {
            throw new RuntimeException(String.valueOf("test2 failed"));
        }
        if (!(strip_chars("123hello123", "123").equals("hello"))) {
            throw new RuntimeException(String.valueOf("test3 failed"));
        }
        if (!(strip("").equals(""))) {
            throw new RuntimeException(String.valueOf("test4 failed"));
        }
    }

    static void main() {
        test_strip();
        System.out.println(strip("   hello   "));
        System.out.println(strip_chars("...world...", "."));
        System.out.println(strip_chars("123hello123", "123"));
        System.out.println(strip(""));
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
