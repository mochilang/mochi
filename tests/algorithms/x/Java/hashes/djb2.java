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

    static int ord(String ch) {
        String upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String lower = "abcdefghijklmnopqrstuvwxyz";
        String digits = "0123456789";
        int idx = index_of(upper, ch);
        if (idx >= 0) {
            return 65 + idx;
        }
        idx = index_of(lower, ch);
        if (idx >= 0) {
            return 97 + idx;
        }
        idx = index_of(digits, ch);
        if (idx >= 0) {
            return 48 + idx;
        }
        if ((ch.equals(" "))) {
            return 32;
        }
        return 0;
    }

    static int djb2(String s) {
        int hash_value = 5381;
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            hash_value = hash_value * 33 + ord(s.substring(i_1, i_1+1));
            hash_value = Math.floorMod(hash_value, (int)4294967296L);
            i_1 = i_1 + 1;
        }
        return hash_value;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(djb2("Algorithms"));
            System.out.println(djb2("scramble bits"));
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
