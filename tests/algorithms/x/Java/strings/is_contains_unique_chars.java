public class Main {

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
        if ((ch.equals("_"))) {
            return 95;
        }
        if ((ch.equals("."))) {
            return 46;
        }
        if ((ch.equals("'"))) {
            return 39;
        }
        return 0;
    }

    static int lshift(int num, int k) {
        int result = num;
        int i_1 = 0;
        while (i_1 < k) {
            result = result * 2;
            i_1 = i_1 + 1;
        }
        return result;
    }

    static int rshift(int num, int k) {
        int result_1 = num;
        int i_2 = 0;
        while (i_2 < k) {
            result_1 = Math.floorDiv((result_1 - (Math.floorMod(result_1, 2))), 2);
            i_2 = i_2 + 1;
        }
        return result_1;
    }

    static boolean is_contains_unique_chars(String input_str) {
        int bitmap = 0;
        int i_3 = 0;
        while (i_3 < _runeLen(input_str)) {
            int code = ord(input_str.substring(i_3, i_3+1));
            if (Math.floorMod(rshift(bitmap, code), 2) == 1) {
                return false;
            }
            bitmap = bitmap + lshift(1, code);
            i_3 = i_3 + 1;
        }
        return true;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(is_contains_unique_chars("I_love.py")));
            System.out.println(_p(is_contains_unique_chars("I don't love Python")));
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
