public class Main {
    static String s1;
    static String s2;

    static boolean is_pangram(String input_str) {
        String[] letters = ((String[])(new String[]{}));
        int i = 0;
        while (i < _runeLen(input_str)) {
            String c = input_str.substring(i, i+1).toLowerCase();
            boolean is_new = !(java.util.Arrays.asList(letters).contains(c));
            if (!(c.equals(" ")) && ("a".compareTo(c) <= 0) && (c.compareTo("z") <= 0) && is_new) {
                letters = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(letters), java.util.stream.Stream.of(c)).toArray(String[]::new)));
            }
            i = i + 1;
        }
        return letters.length == 26;
    }

    static boolean is_pangram_faster(String input_str) {
        String alphabet = "abcdefghijklmnopqrstuvwxyz";
        boolean[] flag = ((boolean[])(new boolean[]{}));
        int i_1 = 0;
        while (i_1 < 26) {
            flag = ((boolean[])(appendBool(flag, false)));
            i_1 = i_1 + 1;
        }
        int j = 0;
        while (j < _runeLen(input_str)) {
            String c_1 = input_str.substring(j, j+1).toLowerCase();
            int k = 0;
            while (k < 26) {
                if ((alphabet.substring(k, k+1).equals(c_1))) {
flag[k] = true;
                    break;
                }
                k = k + 1;
            }
            j = j + 1;
        }
        int t = 0;
        while (t < 26) {
            if (!(Boolean)flag[t]) {
                return false;
            }
            t = t + 1;
        }
        return true;
    }

    static boolean is_pangram_fastest(String input_str) {
        String s = input_str.toLowerCase();
        String alphabet_1 = "abcdefghijklmnopqrstuvwxyz";
        int i_2 = 0;
        while (i_2 < _runeLen(alphabet_1)) {
            String letter = alphabet_1.substring(i_2, i_2+1);
            if (!(s.contains(letter))) {
                return false;
            }
            i_2 = i_2 + 1;
        }
        return true;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            s1 = "The quick brown fox jumps over the lazy dog";
            s2 = "My name is Unknown";
            System.out.println(_p(is_pangram(s1)));
            System.out.println(_p(is_pangram(s2)));
            System.out.println(_p(is_pangram_faster(s1)));
            System.out.println(_p(is_pangram_faster(s2)));
            System.out.println(_p(is_pangram_fastest(s1)));
            System.out.println(_p(is_pangram_fastest(s2)));
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

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
