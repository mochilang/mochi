public class Main {

    static String alternative_string_arrange(String first_str, String second_str) {
        int len1 = _runeLen(first_str);
        int len2 = _runeLen(second_str);
        String res = "";
        int i = 0;
        while (i < len1 || i < len2) {
            if (i < len1) {
                res = res + first_str.substring(i, i+1);
            }
            if (i < len2) {
                res = res + second_str.substring(i, i+1);
            }
            i = i + 1;
        }
        return res;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(alternative_string_arrange("ABCD", "XY"));
            System.out.println(alternative_string_arrange("XY", "ABCD"));
            System.out.println(alternative_string_arrange("AB", "XYZ"));
            System.out.println(alternative_string_arrange("ABC", ""));
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
