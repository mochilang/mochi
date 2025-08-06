public class Main {
    static int MOD_ADLER;

    static int ord(String ch) {
        String lower = "abcdefghijklmnopqrstuvwxyz";
        String upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String digits = "0123456789";
        int i = 0;
        while (i < _runeLen(lower)) {
            if ((lower.substring(i, i+1).equals(ch))) {
                return 97 + i;
            }
            i = i + 1;
        }
        i = 0;
        while (i < _runeLen(upper)) {
            if ((upper.substring(i, i+1).equals(ch))) {
                return 65 + i;
            }
            i = i + 1;
        }
        i = 0;
        while (i < _runeLen(digits)) {
            if ((digits.substring(i, i+1).equals(ch))) {
                return 48 + i;
            }
            i = i + 1;
        }
        if ((ch.equals(" "))) {
            return 32;
        }
        return 0;
    }

    static int adler32(String plain_text) {
        int a = 1;
        int b = 0;
        int i_1 = 0;
        while (i_1 < _runeLen(plain_text)) {
            int code = ord(plain_text.substring(i_1, i_1+1));
            a = Math.floorMod((a + code), MOD_ADLER);
            b = Math.floorMod((b + a), MOD_ADLER);
            i_1 = i_1 + 1;
        }
        return b * 65536 + a;
    }

    static void main() {
        System.out.println(_p(adler32("Algorithms")));
        System.out.println(_p(adler32("go adler em all")));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            MOD_ADLER = 65521;
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
