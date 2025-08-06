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


    static SuffixTree new_suffix_tree(String text) {
        return new SuffixTree(text);
    }

    static boolean search(SuffixTree tree, String pattern) {
        int n = _runeLen(tree.text);
        int m = _runeLen(pattern);
        if (m == 0) {
            return true;
        }
        if (m > n) {
            return false;
        }
        int i = 0;
        while (i <= n - m) {
            if ((tree.text.substring(i, i + m).equals(pattern))) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static void main() {
        String text = "monkey banana";
        SuffixTree suffix_tree = new_suffix_tree(text);
        String[] patterns = ((String[])(new String[]{"ana", "ban", "na", "xyz", "mon"}));
        int i_1 = 0;
        while (i_1 < patterns.length) {
            String pattern = patterns[i_1];
            boolean found = search(suffix_tree, pattern);
            System.out.println("Pattern '" + pattern + "' found: " + _p(found));
            i_1 = i_1 + 1;
        }
    }
    public static void main(String[] args) {
        main();
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
