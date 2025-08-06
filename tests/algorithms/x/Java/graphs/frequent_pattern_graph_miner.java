public class Main {
    static String[][][] EDGE_ARRAY;
    static class NodesData {
        java.util.Map<String,String[]> map;
        String[] keys;
        NodesData(java.util.Map<String,String[]> map, String[] keys) {
            this.map = map;
            this.keys = keys;
        }
        NodesData() {}
        @Override public String toString() {
            return String.format("{'map': %s, 'keys': %s}", String.valueOf(map), String.valueOf(keys));
        }
    }

    static class ClusterData {
        java.util.Map<Integer,String[]> clusters;
        int[] weights;
        ClusterData(java.util.Map<Integer,String[]> clusters, int[] weights) {
            this.clusters = clusters;
            this.weights = weights;
        }
        ClusterData() {}
        @Override public String toString() {
            return String.format("{'clusters': %s, 'weights': %s}", String.valueOf(clusters), String.valueOf(weights));
        }
    }

    static class GraphData {
        java.util.Map<String,String[]> edges;
        String[] keys;
        GraphData(java.util.Map<String,String[]> edges, String[] keys) {
            this.edges = edges;
            this.keys = keys;
        }
        GraphData() {}
        @Override public String toString() {
            return String.format("{'edges': %s, 'keys': %s}", String.valueOf(edges), String.valueOf(keys));
        }
    }

    static String[][] paths = new String[0][];

    static boolean contains(String[] lst, String item) {
        for (String v : lst) {
            if ((v.equals(item))) {
                return true;
            }
        }
        return false;
    }

    static String[] get_distinct_edge(String[][][] edge_array) {
        String[] distinct = ((String[])(new String[]{}));
        for (String[][] row : edge_array) {
            for (String[] item : row) {
                String e = item[0];
                if (!(Boolean)contains(((String[])(distinct)), e)) {
                    distinct = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(distinct), java.util.stream.Stream.of(e)).toArray(String[]::new)));
                }
            }
        }
        return distinct;
    }

    static String get_bitcode(String[][][] edge_array, String de) {
        String bitcode = "";
        int i = 0;
        while (i < edge_array.length) {
            boolean found = false;
            for (String[] item : edge_array[i]) {
                if ((item[0].equals(de))) {
                    found = true;
                    break;
                }
            }
            if (found) {
                bitcode = bitcode + "1";
            } else {
                bitcode = bitcode + "0";
            }
            i = i + 1;
        }
        return bitcode;
    }

    static int count_ones(String s) {
        int c = 0;
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            if ((_substr(s, i_1, i_1 + 1).equals("1"))) {
                c = c + 1;
            }
            i_1 = i_1 + 1;
        }
        return c;
    }

    static java.util.Map<String,String>[] get_frequency_table(String[][][] edge_array) {
        String[] distinct_1 = ((String[])(get_distinct_edge(((String[][][])(edge_array)))));
        java.util.Map<String,String>[] table = ((java.util.Map<String,String>[])((java.util.Map<String,String>[])new java.util.Map[]{}));
        for (String e : distinct_1) {
            String bit = String.valueOf(get_bitcode(((String[][][])(edge_array)), e));
            int cnt = count_ones(bit);
            java.util.Map<String,String> entry = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>(java.util.Map.ofEntries(java.util.Map.entry("edge", e), java.util.Map.entry("count", _p(cnt)), java.util.Map.entry("bit", bit)))));
            table = ((java.util.Map<String,String>[])(appendObj(table, entry)));
        }
        int i_2 = 0;
        while (i_2 < table.length) {
            int max_i = i_2;
            int j = i_2 + 1;
            while (j < table.length) {
                if (String.valueOf(toi(((String)(((java.util.Map)table[j])).get("count")))).compareTo(String.valueOf(toi(((String)(((java.util.Map)table[max_i])).get("count"))))) > 0) {
                    max_i = j;
                }
                j = j + 1;
            }
            java.util.Map<String,String> tmp = table[i_2];
table[i_2] = table[max_i];
table[max_i] = tmp;
            i_2 = i_2 + 1;
        }
        return table;
    }

    static NodesData get_nodes(java.util.Map<String,String>[] freq_table) {
        java.util.Map<String,String[]> nodes = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>()));
        String[] keys = ((String[])(new String[]{}));
        for (java.util.Map<String,String> f : freq_table) {
            String code = ((String)(f).get("bit"));
            String edge = ((String)(f).get("edge"));
            if (((Boolean)(nodes.containsKey(code)))) {
nodes.put(code, ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(((String[])(nodes).get(code))), java.util.stream.Stream.of(edge)).toArray(String[]::new))));
            } else {
nodes.put(code, ((String[])(new String[]{edge})));
                keys = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(keys), java.util.stream.Stream.of(code)).toArray(String[]::new)));
            }
        }
        return new NodesData(nodes, keys);
    }

    static ClusterData get_cluster(NodesData nodes) {
        java.util.Map<Integer,String[]> clusters = ((java.util.Map<Integer,String[]>)(new java.util.LinkedHashMap<Integer, String[]>()));
        int[] weights = ((int[])(new int[]{}));
        int i_3 = 0;
        while (i_3 < nodes.keys.length) {
            String code_1 = nodes.keys[i_3];
            int wt = count_ones(code_1);
            if (((Boolean)(clusters.containsKey(wt)))) {
clusters.put(wt, ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(((String[])(clusters).get(wt))), java.util.stream.Stream.of(code_1)).toArray(String[]::new))));
            } else {
clusters.put(wt, ((String[])(new String[]{code_1})));
                weights = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(weights), java.util.stream.IntStream.of(wt)).toArray()));
            }
            i_3 = i_3 + 1;
        }
        return new ClusterData(clusters, weights);
    }

    static int[] get_support(ClusterData clusters) {
        int[] sup = ((int[])(new int[]{}));
        int i_4 = 0;
        while (i_4 < clusters.weights.length) {
            int w = clusters.weights[i_4];
            sup = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(sup), java.util.stream.IntStream.of(w * 100 / clusters.weights.length)).toArray()));
            i_4 = i_4 + 1;
        }
        return sup;
    }

    static boolean contains_bits(String a, String b) {
        int i_5 = 0;
        while (i_5 < _runeLen(a)) {
            String c1 = _substr(a, i_5, i_5 + 1);
            String c2 = _substr(b, i_5, i_5 + 1);
            if ((c1.equals("1")) && !(c2.equals("1"))) {
                return false;
            }
            i_5 = i_5 + 1;
        }
        return true;
    }

    static int max_cluster_key(ClusterData clusters) {
        int m = 0;
        int i_6 = 0;
        while (i_6 < clusters.weights.length) {
            int w_1 = clusters.weights[i_6];
            if (w_1 > m) {
                m = w_1;
            }
            i_6 = i_6 + 1;
        }
        return m;
    }

    static String[] get_cluster_codes(ClusterData clusters, int wt) {
        if (((Boolean)(clusters.clusters.containsKey(wt)))) {
            return ((String[])(clusters.clusters).get(wt));
        }
        return new String[]{};
    }

    static String[] create_edge(NodesData nodes, java.util.Map<String,String[]> graph, String[] gkeys, ClusterData clusters, int c1, int maxk) {
        String[] keys_1 = ((String[])(gkeys));
        String[] codes1 = ((String[])(get_cluster_codes(clusters, c1)));
        int idx1 = 0;
        while (idx1 < codes1.length) {
            String i_code = codes1[idx1];
            int count = 0;
            int c2_1 = c1 + 1;
            while (c2_1 <= maxk) {
                String[] codes2 = ((String[])(get_cluster_codes(clusters, c2_1)));
                int j_1 = 0;
                while (j_1 < codes2.length) {
                    String j_code = codes2[j_1];
                    if (((Boolean)(contains_bits(i_code, j_code)))) {
                        if (((Boolean)(graph.containsKey(i_code)))) {
graph.put(i_code, ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(((String[])(graph).get(i_code))), java.util.stream.Stream.of(j_code)).toArray(String[]::new))));
                        } else {
graph.put(i_code, ((String[])(new String[]{j_code})));
                            if (!(Boolean)contains(((String[])(keys_1)), i_code)) {
                                keys_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(keys_1), java.util.stream.Stream.of(i_code)).toArray(String[]::new)));
                            }
                        }
                        if (!(Boolean)contains(((String[])(keys_1)), j_code)) {
                            keys_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(keys_1), java.util.stream.Stream.of(j_code)).toArray(String[]::new)));
                        }
                        count = count + 1;
                    }
                    j_1 = j_1 + 1;
                }
                if (count == 0) {
                    c2_1 = c2_1 + 1;
                } else {
                    break;
                }
            }
            idx1 = idx1 + 1;
        }
        return keys_1;
    }

    static GraphData construct_graph(ClusterData clusters, NodesData nodes) {
        int maxk = max_cluster_key(clusters);
        String[] top_codes = ((String[])(get_cluster_codes(clusters, maxk)));
        java.util.Map<String,String[]> graph = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>()));
        String[] keys_2 = ((String[])(new String[]{"Header"}));
graph.put("Header", ((String[])(new String[]{})));
        int i_7 = 0;
        while (i_7 < top_codes.length) {
            String code_2 = top_codes[i_7];
graph.put("Header", ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(((String[])(graph).get("Header"))), java.util.stream.Stream.of(code_2)).toArray(String[]::new))));
graph.put(code_2, ((String[])(new String[]{"Header"})));
            keys_2 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(keys_2), java.util.stream.Stream.of(code_2)).toArray(String[]::new)));
            i_7 = i_7 + 1;
        }
        int c_1 = 1;
        while (c_1 < maxk) {
            keys_2 = ((String[])(create_edge(nodes, graph, ((String[])(keys_2)), clusters, c_1, maxk)));
            c_1 = c_1 + 1;
        }
        return new GraphData(graph, keys_2);
    }

    static String[] copy_list(String[] lst) {
        String[] n = ((String[])(new String[]{}));
        for (String v : lst) {
            n = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(n), java.util.stream.Stream.of(v)).toArray(String[]::new)));
        }
        return n;
    }

    static void my_dfs(java.util.Map<String,String[]> graph, String start, String end, String[] path) {
        String[] new_path = ((String[])(copy_list(((String[])(path)))));
        new_path = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_path), java.util.stream.Stream.of(start)).toArray(String[]::new)));
        if ((start.equals(end))) {
            paths = ((String[][])(appendObj(paths, new_path)));
            return;
        }
        for (String node : ((String[])(graph).get(start))) {
            boolean seen = false;
            for (String p : new_path) {
                if ((p.equals(node))) {
                    seen = true;
                }
            }
            if (!seen) {
                my_dfs(graph, node, end, ((String[])(new_path)));
            }
        }
    }

    static void find_freq_subgraph_given_support(int s, ClusterData clusters, GraphData graph) {
        int k = s * clusters.weights.length / 100;
        String[] codes = ((String[])(get_cluster_codes(clusters, k)));
        int i_8 = 0;
        while (i_8 < codes.length) {
            my_dfs(graph.edges, codes[i_8], "Header", ((String[])(new String[]{})));
            i_8 = i_8 + 1;
        }
    }

    static String[] node_edges(NodesData nodes, String code) {
        return ((String[])(nodes.map).get(code));
    }

    static String[][][] freq_subgraphs_edge_list(String[][] paths, NodesData nodes) {
        String[][][] freq_sub_el = ((String[][][])(new String[][][]{}));
        for (String[] path : paths) {
            String[][] el = ((String[][])(new String[][]{}));
            int j_2 = 0;
            while (j_2 < path.length - 1) {
                String code_3 = path[j_2];
                String[] edge_list = ((String[])(node_edges(nodes, code_3)));
                int e_1 = 0;
                while (e_1 < edge_list.length) {
                    String edge_1 = edge_list[e_1];
                    String a = _substr(edge_1, 0, 1);
                    String b = _substr(edge_1, 1, 2);
                    el = ((String[][])(appendObj(el, new String[]{a, b})));
                    e_1 = e_1 + 1;
                }
                j_2 = j_2 + 1;
            }
            freq_sub_el = ((String[][][])(appendObj(freq_sub_el, el)));
        }
        return freq_sub_el;
    }

    static void print_all(NodesData nodes, int[] support, ClusterData clusters, GraphData graph, String[][][] freq_subgraph_edge_list) {
        System.out.println("\nNodes\n");
        int i_9 = 0;
        while (i_9 < nodes.keys.length) {
            String code_4 = nodes.keys[i_9];
            System.out.println(code_4);
            System.out.println(java.util.Arrays.toString(((String[])(nodes.map).get(code_4))));
            i_9 = i_9 + 1;
        }
        System.out.println("\nSupport\n");
        System.out.println(java.util.Arrays.toString(support));
        System.out.println("\nCluster\n");
        int j_3 = 0;
        while (j_3 < clusters.weights.length) {
            int w_2 = clusters.weights[j_3];
            System.out.println(_p(w_2) + ":" + _p(((String[])(clusters.clusters).get(w_2))));
            j_3 = j_3 + 1;
        }
        System.out.println("\nGraph\n");
        int k_1 = 0;
        while (k_1 < graph.keys.length) {
            String key = graph.keys[k_1];
            System.out.println(key);
            System.out.println(java.util.Arrays.toString(((String[])(graph.edges).get(key))));
            k_1 = k_1 + 1;
        }
        System.out.println("\nEdge List of Frequent subgraphs\n");
        for (String[][] el : freq_subgraph_edge_list) {
            System.out.println(java.util.Arrays.deepToString(el));
        }
    }

    static void main() {
        java.util.Map<String,String>[] frequency_table = ((java.util.Map<String,String>[])(get_frequency_table(((String[][][])(EDGE_ARRAY)))));
        NodesData nodes_1 = get_nodes(((java.util.Map<String,String>[])(frequency_table)));
        ClusterData clusters_1 = get_cluster(nodes_1);
        int[] support = ((int[])(get_support(clusters_1)));
        GraphData graph_1 = construct_graph(clusters_1, nodes_1);
        find_freq_subgraph_given_support(60, clusters_1, graph_1);
        String[][][] freq_subgraph_edge_list = ((String[][][])(freq_subgraphs_edge_list(((String[][])(paths)), nodes_1)));
        print_all(nodes_1, ((int[])(support)), clusters_1, graph_1, ((String[][][])(freq_subgraph_edge_list)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            EDGE_ARRAY = ((String[][][])(new String[][][]{new String[][]{new String[]{"ab", "e1"}, new String[]{"ac", "e3"}, new String[]{"ad", "e5"}, new String[]{"bc", "e4"}, new String[]{"bd", "e2"}, new String[]{"be", "e6"}, new String[]{"bh", "e12"}, new String[]{"cd", "e2"}, new String[]{"ce", "e4"}, new String[]{"de", "e1"}, new String[]{"df", "e8"}, new String[]{"dg", "e5"}, new String[]{"dh", "e10"}, new String[]{"ef", "e3"}, new String[]{"eg", "e2"}, new String[]{"fg", "e6"}, new String[]{"gh", "e6"}, new String[]{"hi", "e3"}}, new String[][]{new String[]{"ab", "e1"}, new String[]{"ac", "e3"}, new String[]{"ad", "e5"}, new String[]{"bc", "e4"}, new String[]{"bd", "e2"}, new String[]{"be", "e6"}, new String[]{"cd", "e2"}, new String[]{"de", "e1"}, new String[]{"df", "e8"}, new String[]{"ef", "e3"}, new String[]{"eg", "e2"}, new String[]{"fg", "e6"}}, new String[][]{new String[]{"ab", "e1"}, new String[]{"ac", "e3"}, new String[]{"bc", "e4"}, new String[]{"bd", "e2"}, new String[]{"de", "e1"}, new String[]{"df", "e8"}, new String[]{"dg", "e5"}, new String[]{"ef", "e3"}, new String[]{"eg", "e2"}, new String[]{"eh", "e12"}, new String[]{"fg", "e6"}, new String[]{"fh", "e10"}, new String[]{"gh", "e6"}}, new String[][]{new String[]{"ab", "e1"}, new String[]{"ac", "e3"}, new String[]{"bc", "e4"}, new String[]{"bd", "e2"}, new String[]{"bh", "e12"}, new String[]{"cd", "e2"}, new String[]{"df", "e8"}, new String[]{"dh", "e10"}}, new String[][]{new String[]{"ab", "e1"}, new String[]{"ac", "e3"}, new String[]{"ad", "e5"}, new String[]{"bc", "e4"}, new String[]{"bd", "e2"}, new String[]{"cd", "e2"}, new String[]{"ce", "e4"}, new String[]{"de", "e1"}, new String[]{"df", "e8"}, new String[]{"dg", "e5"}, new String[]{"ef", "e3"}, new String[]{"eg", "e2"}, new String[]{"fg", "e6"}}}));
            paths = ((String[][])(new String[][]{}));
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
