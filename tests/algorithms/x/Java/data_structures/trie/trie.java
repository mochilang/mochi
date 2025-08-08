public class Main {
    static class Node {
        java.util.Map<String,Integer> children;
        boolean is_leaf;
        Node(java.util.Map<String,Integer> children, boolean is_leaf) {
            this.children = children;
            this.is_leaf = is_leaf;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'children': %s, 'is_leaf': %s}", String.valueOf(children), String.valueOf(is_leaf));
        }
    }

    static class Trie {
        Node[] nodes;
        Trie(Node[] nodes) {
            this.nodes = nodes;
        }
        Trie() {}
        @Override public String toString() {
            return String.format("{'nodes': %s}", String.valueOf(nodes));
        }
    }

    static Trie trie_1;

    static Trie new_trie() {
        return new Trie(new Node[]{new Node(new java.util.LinkedHashMap<String, Integer>(), false)});
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

    static void insert(Trie trie, String word) {
        Node[] nodes = ((Node[])(trie.nodes));
        int curr = 0;
        int i = 0;
        while (i < _runeLen(word)) {
            String ch = word.substring(i, i+1);
            int child_idx = -1;
            java.util.Map<String,Integer> children = nodes[curr].children;
            if (((Boolean)(children.containsKey(ch)))) {
                child_idx = (int)(((int)(children).getOrDefault(ch, 0)));
            } else {
                Node new_node = new Node(new java.util.LinkedHashMap<String, Integer>(), false);
                nodes = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes), java.util.stream.Stream.of(new_node)).toArray(Node[]::new)));
                child_idx = nodes.length - 1;
                java.util.Map<String,Integer> new_children = children;
new_children.put(ch, child_idx);
                Node node = nodes[curr];
node.children = new_children;
nodes[curr] = node;
            }
            curr = child_idx;
            i = i + 1;
        }
        Node node_1 = nodes[curr];
node_1.is_leaf = true;
nodes[curr] = node_1;
trie.nodes = nodes;
    }

    static void insert_many(Trie trie, String[] words) {
        for (String w : words) {
            insert(trie, w);
        }
    }

    static boolean find(Trie trie, String word) {
        Node[] nodes_1 = ((Node[])(trie.nodes));
        int curr_1 = 0;
        int i_1 = 0;
        while (i_1 < _runeLen(word)) {
            String ch_1 = word.substring(i_1, i_1+1);
            java.util.Map<String,Integer> children_1 = nodes_1[curr_1].children;
            if (!(Boolean)(children_1.containsKey(ch_1))) {
                return false;
            }
            curr_1 = (int)(((int)(children_1).getOrDefault(ch_1, 0)));
            i_1 = i_1 + 1;
        }
        Node node_2 = nodes_1[curr_1];
        return node_2.is_leaf;
    }

    static void delete(Trie trie, String word) {
        Node[][] nodes_2 = new Node[1][];
        nodes_2[0] = ((Node[])(trie.nodes));
        java.util.function.BiFunction<Integer,Integer,Boolean>[] _delete = new java.util.function.BiFunction[1];
        _delete[0] = (idx, pos) -> {
        if (pos == _runeLen(word)) {
            Node node_3 = nodes_2[0][idx];
            if (node_3.is_leaf == false) {
                return false;
            }
node_3.is_leaf = false;
nodes_2[0][idx] = node_3;
            return node_3.children.size() == 0;
        }
        Node node_4 = nodes_2[0][idx];
        java.util.Map<String,Integer> children_2 = node_4.children;
        String ch_2 = word.substring(pos, pos+1);
        if (!(Boolean)(children_2.containsKey(ch_2))) {
            return false;
        }
        int child_idx_1 = (int)(((int)(children_2).getOrDefault(ch_2, 0)));
        boolean should_delete = _delete[0].apply(child_idx_1, pos + 1);
        node_4 = nodes_2[0][idx];
        if (((Boolean)(should_delete))) {
            java.util.Map<String,Integer> new_children_1 = remove_key(node_4.children, ch_2);
node_4.children = new_children_1;
nodes_2[0][idx] = node_4;
            return new_children_1.size() == 0 && node_4.is_leaf == false;
        }
nodes_2[0][idx] = node_4;
        return false;
};
        _delete[0].apply(0, 0);
trie.nodes = nodes_2[0];
    }

    static void print_words(Trie trie) {
        java.util.function.BiConsumer<Integer,String>[] dfs = new java.util.function.BiConsumer[1];
        dfs[0] = (idx_1, word) -> {
        Node node_5 = trie.nodes[idx_1];
        if (node_5.is_leaf) {
            System.out.println(word);
        }
        for (String key : node_5.children.keySet()) {
            dfs[0].accept(((int)(node_5.children).getOrDefault(key, 0)), word + key);
        }
};
        dfs[0].accept(0, "");
    }

    static boolean test_trie() {
        String[] words = ((String[])(new String[]{"banana", "bananas", "bandana", "band", "apple", "all", "beast"}));
        Trie trie = new_trie();
        insert_many(trie, ((String[])(words)));
        boolean ok = true;
        for (String w : words) {
            ok = ok && ((Boolean)(find(trie, w)));
        }
        ok = ok && ((Boolean)(find(trie, "banana")));
        boolean t = ((Boolean)(find(trie, "bandanas")));
        ok = ok && (t == false);
        boolean t2 = ((Boolean)(find(trie, "apps")));
        ok = ok && (t2 == false);
        ok = ok && ((Boolean)(find(trie, "apple")));
        ok = ok && ((Boolean)(find(trie, "all")));
        delete(trie, "all");
        boolean t3 = ((Boolean)(find(trie, "all")));
        ok = ok && (t3 == false);
        delete(trie, "banana");
        boolean t4 = ((Boolean)(find(trie, "banana")));
        ok = ok && (t4 == false);
        ok = ok && ((Boolean)(find(trie, "bananas")));
        return ok;
    }

    static void print_results(String msg, boolean passes) {
        if (((Boolean)(passes))) {
            System.out.println(msg + " works!");
        } else {
            System.out.println(msg + " doesn't work :(");
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            trie_1 = new_trie();
            print_results("Testing trie functionality", test_trie());
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
