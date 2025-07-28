public class Main {

    static boolean isInt(String s) {
        if (_runeLen(s) == 0) {
            return false;
        }
        for (int _i = 0; _i < s.length(); _i++) {
            var ch = s.substring(_i, _i + 1);
            if ((ch.compareTo("0") < 0) || (ch.compareTo("9") > 0)) {
                return false;
            }
        }
        return true;
    }

    static void main() {
        System.out.println("Are these strings integers?");
        String v = "1";
        boolean b = false;
        if (isInt(v)) {
            b = true;
        }
        System.out.println("  " + v + " -> " + String.valueOf(b));
        String i = "one";
        System.out.println("  " + i + " -> " + String.valueOf(isInt(i)));
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
}
