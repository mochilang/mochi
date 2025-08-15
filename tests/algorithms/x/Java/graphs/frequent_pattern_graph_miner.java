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
        java.util.Map<Long,String[]> clusters;
        long[] weights;
        ClusterData(java.util.Map<Long,String[]> clusters, long[] weights) {
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
                String e_1 = item[(int)((long)(0))];
                if (!(Boolean)contains(((String[])(distinct)), e_1)) {
                    distinct = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(distinct), java.util.stream.Stream.of(e_1)).toArray(String[]::new)));
                }
            }
        }
        return distinct;
    }

    static String get_bitcode(String[][][] edge_array, String de) {
        String bitcode = "";
        long i_1 = 0L;
        while ((long)(i_1) < (long)(edge_array.length)) {
            boolean found_1 = false;
            for (String[] item : edge_array[(int)((long)(i_1))]) {
                if ((item[(int)((long)(0))].equals(de))) {
                    found_1 = true;
                    break;
                }
            }
            if (found_1) {
                bitcode = bitcode + "1";
            } else {
                bitcode = bitcode + "0";
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return bitcode;
    }

    static long count_ones(String s) {
        long c = 0L;
        long i_3 = 0L;
        while ((long)(i_3) < (long)(_runeLen(s))) {
            if ((_substr(s, (int)((long)(i_3)), (int)((long)((long)(i_3) + 1L))).equals("1"))) {
                c = (long)((long)(c) + 1L);
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
        return c;
    }

    static java.util.Map<String,String>[] get_frequency_table(String[][][] edge_array) {
        String[] distinct_1 = ((String[])(get_distinct_edge(((String[][][])(edge_array)))));
        java.util.Map<String,String>[] table_1 = ((java.util.Map<String,String>[])((java.util.Map<String,String>[])new java.util.Map[]{}));
        for (String e : distinct_1) {
            String bit_1 = String.valueOf(get_bitcode(((String[][][])(edge_array)), e));
            long cnt_1 = (long)(count_ones(bit_1));
            java.util.Map<String,String> entry_1 = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>(java.util.Map.ofEntries(java.util.Map.entry("edge", e), java.util.Map.entry("count", _p(cnt_1)), java.util.Map.entry("bit", bit_1)))));
            table_1 = ((java.util.Map<String,String>[])(appendObj((java.util.Map<String,String>[])table_1, entry_1)));
        }
        long i_5 = 0L;
        while ((long)(i_5) < (long)(table_1.length)) {
            long max_i_1 = (long)(i_5);
            long j_1 = (long)((long)(i_5) + 1L);
            while ((long)(j_1) < (long)(table_1.length)) {
                if ((long)(Integer.parseInt(String.valueOf(((String)(((java.util.Map)table_1[(int)((long)(j_1))])).get("count"))))) > (long)(Integer.parseInt(String.valueOf(((String)(((java.util.Map)table_1[(int)((long)(max_i_1))])).get("count")))))) {
                    max_i_1 = (long)(j_1);
                }
                j_1 = (long)((long)(j_1) + 1L);
            }
            java.util.Map<String,String> tmp_1 = table_1[(int)((long)(i_5))];
table_1[(int)((long)(i_5))] = table_1[(int)((long)(max_i_1))];
table_1[(int)((long)(max_i_1))] = tmp_1;
            i_5 = (long)((long)(i_5) + 1L);
        }
        return table_1;
    }

    static NodesData get_nodes(java.util.Map<String,String>[] freq_table) {
        java.util.Map<String,String[]> nodes = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>()));
        String[] keys_1 = ((String[])(new String[]{}));
        for (java.util.Map<String,String> f : freq_table) {
            String code_1 = ((String)(f).get("bit"));
            String edge_1 = ((String)(f).get("edge"));
            if (nodes.containsKey(code_1)) {
nodes.put(code_1, ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(((String[])(nodes).get(code_1))), java.util.stream.Stream.of(edge_1)).toArray(String[]::new))));
            } else {
nodes.put(code_1, ((String[])(new String[]{edge_1})));
                keys_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(keys_1), java.util.stream.Stream.of(code_1)).toArray(String[]::new)));
            }
        }
        return new NodesData(nodes, keys_1);
    }

    static ClusterData get_cluster(NodesData nodes) {
        java.util.Map<Long,String[]> clusters = ((java.util.Map<Long,String[]>)(new java.util.LinkedHashMap<Long, String[]>()));
        long[] weights_1 = ((long[])(new long[]{}));
        long i_7 = 0L;
        while ((long)(i_7) < (long)(nodes.keys.length)) {
            String code_3 = nodes.keys[(int)((long)(i_7))];
            long wt_1 = (long)(count_ones(code_3));
            if (clusters.containsKey(wt_1)) {
clusters.put(wt_1, ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(((String[])(clusters).get(wt_1))), java.util.stream.Stream.of(code_3)).toArray(String[]::new))));
            } else {
clusters.put(wt_1, ((String[])(new String[]{code_3})));
                weights_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(weights_1), java.util.stream.LongStream.of((long)(wt_1))).toArray()));
            }
            i_7 = (long)((long)(i_7) + 1L);
        }
        return new ClusterData(clusters, weights_1);
    }

    static long[] get_support(ClusterData clusters) {
        long[] sup = ((long[])(new long[]{}));
        long i_9 = 0L;
        while ((long)(i_9) < (long)(clusters.weights.length)) {
            long w_1 = (long)(clusters.weights[(int)((long)(i_9))]);
            sup = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(sup), java.util.stream.LongStream.of((long)((long)((long)(w_1) * 100L) / (long)(clusters.weights.length)))).toArray()));
            i_9 = (long)((long)(i_9) + 1L);
        }
        return sup;
    }

    static boolean contains_bits(String a, String b) {
        long i_10 = 0L;
        while ((long)(i_10) < (long)(_runeLen(a))) {
            String c1_1 = _substr(a, (int)((long)(i_10)), (int)((long)((long)(i_10) + 1L)));
            String c2_1 = _substr(b, (int)((long)(i_10)), (int)((long)((long)(i_10) + 1L)));
            if ((c1_1.equals("1")) && !(c2_1.equals("1"))) {
                return false;
            }
            i_10 = (long)((long)(i_10) + 1L);
        }
        return true;
    }

    static long max_cluster_key(ClusterData clusters) {
        long m = 0L;
        long i_12 = 0L;
        while ((long)(i_12) < (long)(clusters.weights.length)) {
            long w_3 = (long)(clusters.weights[(int)((long)(i_12))]);
            if ((long)(w_3) > (long)(m)) {
                m = (long)(w_3);
            }
            i_12 = (long)((long)(i_12) + 1L);
        }
        return m;
    }

    static String[] get_cluster_codes(ClusterData clusters, long wt) {
        if (clusters.clusters.containsKey(wt)) {
            return ((String[])(clusters.clusters).get(wt));
        }
        return new String[]{};
    }

    static String[] create_edge(NodesData nodes, java.util.Map<String,String[]> graph, String[] gkeys, ClusterData clusters, long c1, long maxk) {
        String[] keys_2 = ((String[])(gkeys));
        String[] codes1_1 = ((String[])(get_cluster_codes(clusters, (long)(c1))));
        long idx1_1 = 0L;
        while ((long)(idx1_1) < (long)(codes1_1.length)) {
            String i_code_1 = codes1_1[(int)((long)(idx1_1))];
            long count_1 = 0L;
            long c2_3 = (long)((long)(c1) + 1L);
            while ((long)(c2_3) <= (long)(maxk)) {
                String[] codes2_1 = ((String[])(get_cluster_codes(clusters, (long)(c2_3))));
                long j_3 = 0L;
                while ((long)(j_3) < (long)(codes2_1.length)) {
                    String j_code_1 = codes2_1[(int)((long)(j_3))];
                    if (contains_bits(i_code_1, j_code_1)) {
                        if (graph.containsKey(i_code_1)) {
graph.put(i_code_1, ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(((String[])(graph).get(i_code_1))), java.util.stream.Stream.of(j_code_1)).toArray(String[]::new))));
                        } else {
graph.put(i_code_1, ((String[])(new String[]{j_code_1})));
                            if (!(Boolean)contains(((String[])(keys_2)), i_code_1)) {
                                keys_2 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(keys_2), java.util.stream.Stream.of(i_code_1)).toArray(String[]::new)));
                            }
                        }
                        if (!(Boolean)contains(((String[])(keys_2)), j_code_1)) {
                            keys_2 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(keys_2), java.util.stream.Stream.of(j_code_1)).toArray(String[]::new)));
                        }
                        count_1 = (long)((long)(count_1) + 1L);
                    }
                    j_3 = (long)((long)(j_3) + 1L);
                }
                if ((long)(count_1) == 0L) {
                    c2_3 = (long)((long)(c2_3) + 1L);
                } else {
                    break;
                }
            }
            idx1_1 = (long)((long)(idx1_1) + 1L);
        }
        return keys_2;
    }

    static GraphData construct_graph(ClusterData clusters, NodesData nodes) {
        long maxk = (long)(max_cluster_key(clusters));
        String[] top_codes_1 = ((String[])(get_cluster_codes(clusters, (long)(maxk))));
        java.util.Map<String,String[]> graph_1 = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>()));
        String[] keys_4 = ((String[])(new String[]{"Header"}));
graph_1.put("Header", ((String[])(new String[]{})));
        long i_14 = 0L;
        while ((long)(i_14) < (long)(top_codes_1.length)) {
            String code_5 = top_codes_1[(int)((long)(i_14))];
graph_1.put("Header", ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(((String[])(graph_1).get("Header"))), java.util.stream.Stream.of(code_5)).toArray(String[]::new))));
graph_1.put(code_5, ((String[])(new String[]{"Header"})));
            keys_4 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(keys_4), java.util.stream.Stream.of(code_5)).toArray(String[]::new)));
            i_14 = (long)((long)(i_14) + 1L);
        }
        long c_2 = 1L;
        while ((long)(c_2) < (long)(maxk)) {
            keys_4 = ((String[])(create_edge(nodes, graph_1, ((String[])(keys_4)), clusters, (long)(c_2), (long)(maxk))));
            c_2 = (long)((long)(c_2) + 1L);
        }
        return new GraphData(graph_1, keys_4);
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
            paths = ((String[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(paths), java.util.stream.Stream.of(new String[][]{new_path})).toArray(String[][]::new)));
            return;
        }
        for (String node : ((String[])(graph).get(start))) {
            boolean seen_1 = false;
            for (String p : new_path) {
                if ((p.equals(node))) {
                    seen_1 = true;
                }
            }
            if (!seen_1) {
                my_dfs(graph, node, end, ((String[])(new_path)));
            }
        }
    }

    static void find_freq_subgraph_given_support(long s, ClusterData clusters, GraphData graph) {
        long k = (long)((long)((long)(s) * (long)(clusters.weights.length)) / 100L);
        String[] codes_1 = ((String[])(get_cluster_codes(clusters, (long)(k))));
        long i_16 = 0L;
        while ((long)(i_16) < (long)(codes_1.length)) {
            my_dfs(graph.edges, codes_1[(int)((long)(i_16))], "Header", ((String[])(new String[]{})));
            i_16 = (long)((long)(i_16) + 1L);
        }
    }

    static String[] node_edges(NodesData nodes, String code) {
        return ((String[])(nodes.map).get(code));
    }

    static String[][][] freq_subgraphs_edge_list(String[][] paths, NodesData nodes) {
        String[][][] freq_sub_el = ((String[][][])(new String[][][]{}));
        for (String[] path : paths) {
            String[][] el_1 = ((String[][])(new String[][]{}));
            long j_5 = 0L;
            while ((long)(j_5) < (long)((long)(path.length) - 1L)) {
                String code_7 = path[(int)((long)(j_5))];
                String[] edge_list_1 = ((String[])(node_edges(nodes, code_7)));
                long e_3 = 0L;
                while ((long)(e_3) < (long)(edge_list_1.length)) {
                    String edge_3 = edge_list_1[(int)((long)(e_3))];
                    String a_1 = _substr(edge_3, (int)((long)(0)), (int)((long)(1)));
                    String b_1 = _substr(edge_3, (int)((long)(1)), (int)((long)(2)));
                    el_1 = ((String[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(el_1), java.util.stream.Stream.of(new String[][]{new String[]{a_1, b_1}})).toArray(String[][]::new)));
                    e_3 = (long)((long)(e_3) + 1L);
                }
                j_5 = (long)((long)(j_5) + 1L);
            }
            freq_sub_el = ((String[][][])(java.util.stream.Stream.concat(java.util.Arrays.stream(freq_sub_el), java.util.stream.Stream.of(new String[][][]{el_1})).toArray(String[][][]::new)));
        }
        return freq_sub_el;
    }

    static void print_all(NodesData nodes, long[] support, ClusterData clusters, GraphData graph, String[][][] freq_subgraph_edge_list) {
        System.out.println("\nNodes\n");
        long i_18 = 0L;
        while ((long)(i_18) < (long)(nodes.keys.length)) {
            String code_9 = nodes.keys[(int)((long)(i_18))];
            System.out.println(code_9);
            System.out.println(java.util.Arrays.toString(((String[])(nodes.map).get(code_9))));
            i_18 = (long)((long)(i_18) + 1L);
        }
        System.out.println("\nSupport\n");
        System.out.println(java.util.Arrays.toString(support));
        System.out.println("\nCluster\n");
        long j_7 = 0L;
        while ((long)(j_7) < (long)(clusters.weights.length)) {
            long w_5 = (long)(clusters.weights[(int)((long)(j_7))]);
            System.out.println(_p(w_5) + ":" + _p(((String[])(clusters.clusters).get(w_5))));
            j_7 = (long)((long)(j_7) + 1L);
        }
        System.out.println("\nGraph\n");
        long k_2 = 0L;
        while ((long)(k_2) < (long)(graph.keys.length)) {
            String key_1 = graph.keys[(int)((long)(k_2))];
            System.out.println(key_1);
            System.out.println(java.util.Arrays.toString(((String[])(graph.edges).get(key_1))));
            k_2 = (long)((long)(k_2) + 1L);
        }
        System.out.println("\nEdge List of Frequent subgraphs\n");
        for (String[][] el : freq_subgraph_edge_list) {
            System.out.println(java.util.Arrays.deepToString(el));
        }
    }

    static void main() {
        java.util.Map<String,String>[] frequency_table = ((java.util.Map<String,String>[])(get_frequency_table(((String[][][])(EDGE_ARRAY)))));
        NodesData nodes_2 = get_nodes(((java.util.Map<String,String>[])(frequency_table)));
        ClusterData clusters_2 = get_cluster(nodes_2);
        long[] support_1 = ((long[])(get_support(clusters_2)));
        GraphData graph_3 = construct_graph(clusters_2, nodes_2);
        find_freq_subgraph_given_support(60L, clusters_2, graph_3);
        String[][][] freq_subgraph_edge_list_1 = ((String[][][])(freq_subgraphs_edge_list(((String[][])(paths)), nodes_2)));
        print_all(nodes_2, ((long[])(support_1)), clusters_2, graph_3, ((String[][][])(freq_subgraph_edge_list_1)));
    }
    public static void main(String[] args) {
        EDGE_ARRAY = ((String[][][])(new String[][][]{new String[][]{new String[]{"ab", "e1"}, new String[]{"ac", "e3"}, new String[]{"ad", "e5"}, new String[]{"bc", "e4"}, new String[]{"bd", "e2"}, new String[]{"be", "e6"}, new String[]{"bh", "e12"}, new String[]{"cd", "e2"}, new String[]{"ce", "e4"}, new String[]{"de", "e1"}, new String[]{"df", "e8"}, new String[]{"dg", "e5"}, new String[]{"dh", "e10"}, new String[]{"ef", "e3"}, new String[]{"eg", "e2"}, new String[]{"fg", "e6"}, new String[]{"gh", "e6"}, new String[]{"hi", "e3"}}, new String[][]{new String[]{"ab", "e1"}, new String[]{"ac", "e3"}, new String[]{"ad", "e5"}, new String[]{"bc", "e4"}, new String[]{"bd", "e2"}, new String[]{"be", "e6"}, new String[]{"cd", "e2"}, new String[]{"de", "e1"}, new String[]{"df", "e8"}, new String[]{"ef", "e3"}, new String[]{"eg", "e2"}, new String[]{"fg", "e6"}}, new String[][]{new String[]{"ab", "e1"}, new String[]{"ac", "e3"}, new String[]{"bc", "e4"}, new String[]{"bd", "e2"}, new String[]{"de", "e1"}, new String[]{"df", "e8"}, new String[]{"dg", "e5"}, new String[]{"ef", "e3"}, new String[]{"eg", "e2"}, new String[]{"eh", "e12"}, new String[]{"fg", "e6"}, new String[]{"fh", "e10"}, new String[]{"gh", "e6"}}, new String[][]{new String[]{"ab", "e1"}, new String[]{"ac", "e3"}, new String[]{"bc", "e4"}, new String[]{"bd", "e2"}, new String[]{"bh", "e12"}, new String[]{"cd", "e2"}, new String[]{"df", "e8"}, new String[]{"dh", "e10"}}, new String[][]{new String[]{"ab", "e1"}, new String[]{"ac", "e3"}, new String[]{"ad", "e5"}, new String[]{"bc", "e4"}, new String[]{"bd", "e2"}, new String[]{"cd", "e2"}, new String[]{"ce", "e4"}, new String[]{"de", "e1"}, new String[]{"df", "e8"}, new String[]{"dg", "e5"}, new String[]{"ef", "e3"}, new String[]{"eg", "e2"}, new String[]{"fg", "e6"}}}));
        paths = ((String[][])(new String[][]{}));
        main();
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
        int len = _runeLen(s);
        if (i < 0) i = 0;
        if (j > len) j = len;
        if (i > j) i = j;
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
