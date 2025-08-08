public class Main {

    static int index_of(String s, String ch) {
        int i = 0;
        while (i < _runeLen(s)) {
            if ((s.substring(i, i+1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static String lower(String word) {
        String upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String lower_chars = "abcdefghijklmnopqrstuvwxyz";
        String result = "";
        int i_1 = 0;
        while (i_1 < _runeLen(word)) {
            String c = word.substring(i_1, i_1+1);
            int idx = index_of(upper, c);
            if (idx >= 0) {
                result = result + lower_chars.substring(idx, idx + 1);
            } else {
                result = result + c;
            }
            i_1 = i_1 + 1;
        }
        return result;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println("wow".toLowerCase());
            System.out.println("HellZo".toLowerCase());
            System.out.println("WHAT".toLowerCase());
            System.out.println("wh[]32".toLowerCase());
            System.out.println("whAT".toLowerCase());
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
