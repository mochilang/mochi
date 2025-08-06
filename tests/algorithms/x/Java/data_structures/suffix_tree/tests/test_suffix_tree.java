public class Main {
    static class SuffixTree {
        String text;
        SuffixTree(String text) {
            this.text = text;
        }
        SuffixTree() {}
        @Override public String toString() {
            return String.format("{'text': '%s'}", String.valueOf(text));
        }
    }

    static String text;
    static SuffixTree st;
    static String[] patterns_exist;
    static int i_1 = 0;
    static String[] patterns_none;
    static String[] substrings;

    static SuffixTree suffix_tree_new(String text) {
        return new SuffixTree(text);
    }

    static boolean suffix_tree_search(SuffixTree st, String pattern) {
        if (_runeLen(pattern) == 0) {
            return true;
        }
        int i = 0;
        int n = _runeLen(st.text);
        int m = _runeLen(pattern);
        while (i <= n - m) {
            int j = 0;
            boolean found = true;
            while (j < m) {
                if (!(st.text.substring(i + j, i + j+1).equals(pattern.substring(j, j+1)))) {
                    found = false;
                    break;
                }
                j = j + 1;
            }
            if (((Boolean)(found))) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            text = "banana";
            st = suffix_tree_new(text);
            patterns_exist = ((String[])(new String[]{"ana", "ban", "na"}));
            i_1 = 0;
            while (i_1 < patterns_exist.length) {
                System.out.println(_p(suffix_tree_search(st, patterns_exist[i_1])));
                i_1 = i_1 + 1;
            }
            patterns_none = ((String[])(new String[]{"xyz", "apple", "cat"}));
            i_1 = 0;
            while (i_1 < patterns_none.length) {
                System.out.println(_p(suffix_tree_search(st, patterns_none[i_1])));
                i_1 = i_1 + 1;
            }
            System.out.println(_p(suffix_tree_search(st, "")));
            System.out.println(_p(suffix_tree_search(st, text)));
            substrings = ((String[])(new String[]{"ban", "ana", "a", "na"}));
            i_1 = 0;
            while (i_1 < substrings.length) {
                System.out.println(_p(suffix_tree_search(st, substrings[i_1])));
                i_1 = i_1 + 1;
            }
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
