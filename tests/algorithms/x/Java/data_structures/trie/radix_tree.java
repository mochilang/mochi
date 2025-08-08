public class Main {
    static class RadixNode {
        String prefix;
        boolean is_leaf;
        java.util.Map<String,Integer> children;
        RadixNode(String prefix, boolean is_leaf, java.util.Map<String,Integer> children) {
            this.prefix = prefix;
            this.is_leaf = is_leaf;
            this.children = children;
        }
        RadixNode() {}
        @Override public String toString() {
            return String.format("{'prefix': '%s', 'is_leaf': %s, 'children': %s}", String.valueOf(prefix), String.valueOf(is_leaf), String.valueOf(children));
        }
    }

    static class RadixTree {
        RadixNode[] nodes;
        RadixTree(RadixNode[] nodes) {
            this.nodes = nodes;
        }
        RadixTree() {}
        @Override public String toString() {
            return String.format("{'nodes': %s}", String.valueOf(nodes));
        }
    }

    static class MatchResult {
        String common;
        String rem_prefix;
        String rem_word;
        MatchResult(String common, String rem_prefix, String rem_word) {
            this.common = common;
            this.rem_prefix = rem_prefix;
            this.rem_word = rem_word;
        }
        MatchResult() {}
        @Override public String toString() {
            return String.format("{'common': '%s', 'rem_prefix': '%s', 'rem_word': '%s'}", String.valueOf(common), String.valueOf(rem_prefix), String.valueOf(rem_word));
        }
    }


    static RadixNode new_node(String prefix, boolean is_leaf) {
        return new RadixNode(prefix, is_leaf, new java.util.LinkedHashMap<String, Integer>());
    }

    static RadixTree new_tree() {
        RadixNode[] nodes = ((RadixNode[])(new RadixNode[]{new_node("", false)}));
        return new RadixTree(nodes);
    }

    static MatchResult match_prefix(RadixNode node, String word) {
        int x = 0;
        String p = node.prefix;
        String w = word;
        int min_len = _runeLen(p);
        if (_runeLen(w) < min_len) {
            min_len = _runeLen(w);
        }
        while (x < min_len) {
            if (!(_substr(p, x, x + 1).equals(_substr(w, x, x + 1)))) {
                break;
            }
            x = x + 1;
        }
        String common = _substr(p, 0, x);
        String rem_prefix = _substr(p, x, _runeLen(p));
        String rem_word = _substr(w, x, _runeLen(w));
        return new MatchResult(common, rem_prefix, rem_word);
    }

    static void insert_many(RadixTree tree, String[] words) {
        for (String w : words) {
            insert(tree, 0, w);
        }
    }

    static void insert(RadixTree tree, int idx, String word) {
        RadixNode[] nodes_1 = ((RadixNode[])(tree.nodes));
        RadixNode node = nodes_1[idx];
        if (((node.prefix.equals(word))) && (!node.is_leaf)) {
node.is_leaf = true;
nodes_1[idx] = node;
tree.nodes = nodes_1;
            return;
        }
        String first = _substr(word, 0, 1);
        java.util.Map<String,Integer> children = node.children;
        if (!(Boolean)has_key(children, first)) {
            int new_idx = nodes_1.length;
            nodes_1 = ((RadixNode[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes_1), java.util.stream.Stream.of(new_node(word, true))).toArray(RadixNode[]::new)));
children.put(first, new_idx);
node.children = children;
nodes_1[idx] = node;
tree.nodes = nodes_1;
            return;
        }
        int child_idx = (int)(((int)(children).getOrDefault(first, 0)));
        RadixNode child = nodes_1[child_idx];
        MatchResult res = match_prefix(child, word);
        if ((res.rem_prefix.equals(""))) {
            insert(tree, child_idx, res.rem_word);
            return;
        }
child.prefix = res.rem_prefix;
nodes_1[child_idx] = child;
        java.util.Map<String,Integer> new_children = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
new_children.put(_substr(res.rem_prefix, 0, 1), child_idx);
        int new_idx_1 = nodes_1.length;
        nodes_1 = ((RadixNode[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes_1), java.util.stream.Stream.of(new_node(res.common, false))).toArray(RadixNode[]::new)));
        if ((res.rem_word.equals(""))) {
        } else {
            insert(tree, new_idx_1, res.rem_word);
        }
children.put(first, new_idx_1);
node.children = children;
nodes_1[idx] = node;
tree.nodes = nodes_1;
    }

    static boolean find(RadixTree tree, int idx, String word) {
        RadixNode[] nodes_2 = ((RadixNode[])(tree.nodes));
        RadixNode node_1 = nodes_2[idx];
        String first_1 = _substr(word, 0, 1);
        java.util.Map<String,Integer> children_1 = node_1.children;
        if (!(Boolean)has_key(children_1, first_1)) {
            return false;
        }
        int child_idx_1 = (int)(((int)(children_1).getOrDefault(first_1, 0)));
        RadixNode child_1 = nodes_2[child_idx_1];
        MatchResult res_1 = match_prefix(child_1, word);
        if (!(res_1.rem_prefix.equals(""))) {
            return false;
        }
        if ((res_1.rem_word.equals(""))) {
            return child_1.is_leaf;
        }
        return find(tree, child_idx_1, res_1.rem_word);
    }

    static java.util.Map<String,Integer> remove_key(java.util.Map<String,Integer> m, String k) {
        java.util.Map<String,Integer> out = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
        for (String key : m.keySet()) {
            if (!(key.equals(k))) {
out.put(key, (int)(((int)(m).getOrDefault(key, 0))));
            }
        }
        return out;
    }

    static boolean has_key(java.util.Map<String,Integer> m, String k) {
        for (String key : m.keySet()) {
            if ((key.equals(k))) {
                return true;
            }
        }
        return false;
    }

    static boolean delete(RadixTree tree, int idx, String word) {
        RadixNode[] nodes_3 = ((RadixNode[])(tree.nodes));
        RadixNode node_2 = nodes_3[idx];
        String first_2 = _substr(word, 0, 1);
        java.util.Map<String,Integer> children_2 = node_2.children;
        if (!(Boolean)has_key(children_2, first_2)) {
            return false;
        }
        int child_idx_2 = (int)(((int)(children_2).getOrDefault(first_2, 0)));
        RadixNode child_2 = nodes_3[child_idx_2];
        MatchResult res_2 = match_prefix(child_2, word);
        if (!(res_2.rem_prefix.equals(""))) {
            return false;
        }
        if (!(res_2.rem_word.equals(""))) {
            boolean deleted = delete(tree, child_idx_2, res_2.rem_word);
            if (((Boolean)(deleted))) {
                nodes_3 = ((RadixNode[])(tree.nodes));
                node_2 = nodes_3[idx];
            }
            return deleted;
        }
        if (!child_2.is_leaf) {
            return false;
        }
        if (child_2.children.size() == 0) {
            children_2 = remove_key(children_2, first_2);
node_2.children = children_2;
nodes_3[idx] = node_2;
tree.nodes = nodes_3;
            if ((children_2.size() == 1) && (!node_2.is_leaf)) {
                String only_key = "";
                for (String k : children_2.keySet()) {
                    only_key = k;
                }
                int merge_idx = (int)(((int)(children_2).getOrDefault(only_key, 0)));
                RadixNode merge_node = nodes_3[merge_idx];
node_2.is_leaf = merge_node.is_leaf;
node_2.prefix = node_2.prefix + merge_node.prefix;
node_2.children = merge_node.children;
nodes_3[idx] = node_2;
tree.nodes = nodes_3;
            }
        } else         if (child_2.children.size() > 1) {
child_2.is_leaf = false;
nodes_3[child_idx_2] = child_2;
tree.nodes = nodes_3;
        } else {
            String only_key_1 = "";
            for (String k : child_2.children.keySet()) {
                only_key_1 = k;
            }
            int merge_idx_1 = (int)(((int)(child_2.children).getOrDefault(only_key_1, 0)));
            RadixNode merge_node_1 = nodes_3[merge_idx_1];
child_2.is_leaf = merge_node_1.is_leaf;
child_2.prefix = child_2.prefix + merge_node_1.prefix;
child_2.children = merge_node_1.children;
nodes_3[child_idx_2] = child_2;
tree.nodes = nodes_3;
        }
        return true;
    }

    static void print_tree(RadixTree tree, int idx, int height) {
        RadixNode[] nodes_4 = ((RadixNode[])(tree.nodes));
        RadixNode node_3 = nodes_4[idx];
        if (!(node_3.prefix.equals(""))) {
            String line = "";
            int i = 0;
            while (i < height) {
                line = line + "-";
                i = i + 1;
            }
            line = line + " " + node_3.prefix;
            if (node_3.is_leaf) {
                line = line + "  (leaf)";
            }
            System.out.println(line);
        }
        java.util.Map<String,Integer> children_3 = node_3.children;
        for (String k : children_3.keySet()) {
            int child_idx_3 = (int)(((int)(children_3).getOrDefault(k, 0)));
            print_tree(tree, child_idx_3, height + 1);
        }
    }

    static boolean test_trie() {
        String[] words = ((String[])(new String[]{"banana", "bananas", "bandana", "band", "apple", "all", "beast"}));
        RadixTree tree = new_tree();
        insert_many(tree, ((String[])(words)));
        boolean ok = true;
        for (String w : words) {
            if (!(Boolean)find(tree, 0, w)) {
                ok = false;
            }
        }
        if (((Boolean)(find(tree, 0, "bandanas")))) {
            ok = false;
        }
        if (((Boolean)(find(tree, 0, "apps")))) {
            ok = false;
        }
        delete(tree, 0, "all");
        if (((Boolean)(find(tree, 0, "all")))) {
            ok = false;
        }
        delete(tree, 0, "banana");
        if (((Boolean)(find(tree, 0, "banana")))) {
            ok = false;
        }
        if (!(Boolean)find(tree, 0, "bananas")) {
            ok = false;
        }
        return ok;
    }

    static void pytests() {
        if (!(Boolean)test_trie()) {
            throw new RuntimeException(String.valueOf("test failed"));
        }
    }

    static void main() {
        RadixTree tree_1 = new_tree();
        String[] words_1 = ((String[])(new String[]{"banana", "bananas", "bandanas", "bandana", "band", "apple", "all", "beast"}));
        insert_many(tree_1, ((String[])(words_1)));
        System.out.println("Words: " + _p(words_1));
        System.out.println("Tree:");
        print_tree(tree_1, 0, 0);
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
