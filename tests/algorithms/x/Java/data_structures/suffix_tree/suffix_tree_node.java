public class Main {
    static class SuffixTreeNode {
        java.util.Map<String,Integer> children;
        boolean is_end_of_string;
        int start;
        int end;
        int suffix_link;
        SuffixTreeNode(java.util.Map<String,Integer> children, boolean is_end_of_string, int start, int end, int suffix_link) {
            this.children = children;
            this.is_end_of_string = is_end_of_string;
            this.start = start;
            this.end = end;
            this.suffix_link = suffix_link;
        }
        SuffixTreeNode() {}
        @Override public String toString() {
            return String.format("{'children': %s, 'is_end_of_string': %s, 'start': %s, 'end': %s, 'suffix_link': %s}", String.valueOf(children), String.valueOf(is_end_of_string), String.valueOf(start), String.valueOf(end), String.valueOf(suffix_link));
        }
    }

    static SuffixTreeNode root;
    static SuffixTreeNode leaf;
    static SuffixTreeNode[] nodes = new SuffixTreeNode[0];
    static SuffixTreeNode root_check = null;
    static SuffixTreeNode leaf_check = null;

    static SuffixTreeNode new_suffix_tree_node(java.util.Map<String,Integer> children, boolean is_end_of_string, int start, int end, int suffix_link) {
        return new SuffixTreeNode(children, is_end_of_string, start, end, suffix_link);
    }

    static SuffixTreeNode empty_suffix_tree_node() {
        return new_suffix_tree_node(((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>())), false, 0 - 1, 0 - 1, 0 - 1);
    }

    static boolean has_key(java.util.Map<String,Integer> m, String k) {
        for (String key : m.keySet()) {
            if ((key.equals(k))) {
                return true;
            }
        }
        return false;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            root = new_suffix_tree_node(((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 1))))), false, 0 - 1, 0 - 1, 0 - 1);
            leaf = new_suffix_tree_node(((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>())), true, 0, 2, 0);
            nodes = ((SuffixTreeNode[])(new SuffixTreeNode[]{root, leaf}));
            root_check = nodes[0];
            leaf_check = nodes[1];
            System.out.println(_p(has_key(root_check.children, "a")));
            System.out.println(_p(leaf_check.is_end_of_string));
            System.out.println(_p(leaf_check.start));
            System.out.println(_p(leaf_check.end));
            System.out.println(_p(leaf_check.suffix_link));
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
