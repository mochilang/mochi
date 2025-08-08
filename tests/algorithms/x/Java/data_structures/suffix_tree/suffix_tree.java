public class Main {
    static class Node {
        java.util.Map<String,Integer> children;
        boolean is_end_of_string;
        int start;
        int end;
        Node(java.util.Map<String,Integer> children, boolean is_end_of_string, int start, int end) {
            this.children = children;
            this.is_end_of_string = is_end_of_string;
            this.start = start;
            this.end = end;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'children': %s, 'is_end_of_string': %s, 'start': %s, 'end': %s}", String.valueOf(children), String.valueOf(is_end_of_string), String.valueOf(start), String.valueOf(end));
        }
    }

    static class SuffixTree {
        String text;
        Node[] nodes;
        SuffixTree(String text, Node[] nodes) {
            this.text = text;
            this.nodes = nodes;
        }
        SuffixTree() {}
        @Override public String toString() {
            return String.format("{'text': '%s', 'nodes': %s}", String.valueOf(text), String.valueOf(nodes));
        }
    }

    static SuffixTree st;

    static Node new_node() {
        return new Node(new java.util.LinkedHashMap<String, Integer>(), false, -1, -1);
    }

    static boolean has_key(java.util.Map<String,Integer> m, String k) {
        for (String key : m.keySet()) {
            if ((key.equals(k))) {
                return true;
            }
        }
        return false;
    }

    static SuffixTree add_suffix(SuffixTree tree, String suffix, int index) {
        Node[] nodes = ((Node[])(tree.nodes));
        int node_idx = 0;
        int j = 0;
        while (j < _runeLen(suffix)) {
            String ch = _substr(suffix, j, j + 1);
            Node node = nodes[node_idx];
            java.util.Map<String,Integer> children = node.children;
            if (!(Boolean)has_key(children, ch)) {
                nodes = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes), java.util.stream.Stream.of(new_node())).toArray(Node[]::new)));
                int new_idx = nodes.length - 1;
children.put(ch, new_idx);
            }
node.children = children;
nodes[node_idx] = node;
            node_idx = (int)(((int)(children).getOrDefault(ch, 0)));
            j = j + 1;
        }
        Node node_1 = nodes[node_idx];
node_1.is_end_of_string = true;
node_1.start = index;
node_1.end = index + _runeLen(suffix) - 1;
nodes[node_idx] = node_1;
tree.nodes = nodes;
        return tree;
    }

    static SuffixTree build_suffix_tree(SuffixTree tree) {
        String text = tree.text;
        int n = _runeLen(text);
        int i = 0;
        SuffixTree t = tree;
        while (i < n) {
            String suffix = "";
            int k = i;
            while (k < n) {
                suffix = suffix + _substr(text, k, k + 1);
                k = k + 1;
            }
            t = add_suffix(t, suffix, i);
            i = i + 1;
        }
        return t;
    }

    static SuffixTree new_suffix_tree(String text) {
        SuffixTree tree = new SuffixTree(text, new Node[]{});
tree.nodes = java.util.stream.Stream.concat(java.util.Arrays.stream(tree.nodes), java.util.stream.Stream.of(new_node())).toArray(Node[]::new);
        tree = build_suffix_tree(tree);
        return tree;
    }

    static boolean search(SuffixTree tree, String pattern) {
        int node_idx_1 = 0;
        int i_1 = 0;
        Node[] nodes_1 = ((Node[])(tree.nodes));
        while (i_1 < _runeLen(pattern)) {
            String ch_1 = _substr(pattern, i_1, i_1 + 1);
            Node node_2 = nodes_1[node_idx_1];
            java.util.Map<String,Integer> children_1 = node_2.children;
            if (!(Boolean)has_key(children_1, ch_1)) {
                return false;
            }
            node_idx_1 = (int)(((int)(children_1).getOrDefault(ch_1, 0)));
            i_1 = i_1 + 1;
        }
        return true;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            st = new_suffix_tree("bananas");
            System.out.println(_p(search(st, "ana")));
            System.out.println(_p(search(st, "apple")));
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

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
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
