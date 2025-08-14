public class Main {
    static class Graph {
        java.util.Map<String,String[]> adj;
        boolean directed;
        Graph(java.util.Map<String,String[]> adj, boolean directed) {
            this.adj = adj;
            this.directed = directed;
        }
        Graph() {}
        @Override public String toString() {
            return String.format("{'adj': %s, 'directed': %s}", String.valueOf(adj), String.valueOf(directed));
        }
    }


    static Graph create_graph(String[] vertices, String[][] edges, boolean directed) {
        java.util.Map<String,String[]> adj = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>()));
        for (String v : vertices) {
adj.put(v, ((String[])(new String[]{})));
        }
        for (String[] e : edges) {
            String s_1 = e[(int)((long)(0))];
            String d_1 = e[(int)((long)(1))];
            if (!(adj.containsKey(s_1))) {
adj.put(s_1, ((String[])(new String[]{})));
            }
            if (!(adj.containsKey(d_1))) {
adj.put(d_1, ((String[])(new String[]{})));
            }
adj.put(s_1, ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(((String[])(adj).get(s_1))), java.util.stream.Stream.of(d_1)).toArray(String[]::new))));
            if (!(Boolean)directed) {
adj.put(d_1, ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(((String[])(adj).get(d_1))), java.util.stream.Stream.of(s_1)).toArray(String[]::new))));
            }
        }
        return new Graph(adj, directed);
    }

    static Graph add_vertex(Graph graph, String v) {
        if (graph.adj.containsKey(v)) {
            throw new RuntimeException(String.valueOf("vertex exists"));
        }
        java.util.Map<String,String[]> adj_2 = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>()));
        for (String k : graph.adj.keySet()) {
adj_2.put(k, (String[])(((String[])(graph.adj).get(k))));
        }
adj_2.put(v, ((String[])(new String[]{})));
        return new Graph(adj_2, graph.directed);
    }

    static String[] remove_from_list(String[] lst, String value) {
        String[] res = ((String[])(new String[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(lst.length)) {
            if (!(lst[(int)((long)(i_1))].equals(value))) {
                res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(lst[(int)((long)(i_1))])).toArray(String[]::new)));
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return res;
    }

    static java.util.Map<String,String[]> remove_key(java.util.Map<String,String[]> m, String key) {
        java.util.Map<String,String[]> res_1 = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>()));
        for (String k : m.keySet()) {
            if (!(k.equals(key))) {
res_1.put(k, (String[])(((String[])(m).get(k))));
            }
        }
        return res_1;
    }

    static Graph add_edge(Graph graph, String s, String d) {
        if (((!(graph.adj.containsKey(s))) || (!(graph.adj.containsKey(d))))) {
            throw new RuntimeException(String.valueOf("vertex missing"));
        }
        if (contains_edge(graph, s, d)) {
            throw new RuntimeException(String.valueOf("edge exists"));
        }
        java.util.Map<String,String[]> adj_4 = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>()));
        for (String k : graph.adj.keySet()) {
adj_4.put(k, (String[])(((String[])(graph.adj).get(k))));
        }
        String[] list_s_1 = (String[])(((String[])(adj_4).get(s)));
        list_s_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(list_s_1), java.util.stream.Stream.of(d)).toArray(String[]::new)));
adj_4.put(s, ((String[])(list_s_1)));
        if (!graph.directed) {
            String[] list_d_1 = (String[])(((String[])(adj_4).get(d)));
            list_d_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(list_d_1), java.util.stream.Stream.of(s)).toArray(String[]::new)));
adj_4.put(d, ((String[])(list_d_1)));
        }
        return new Graph(adj_4, graph.directed);
    }

    static Graph remove_edge(Graph graph, String s, String d) {
        if (((!(graph.adj.containsKey(s))) || (!(graph.adj.containsKey(d))))) {
            throw new RuntimeException(String.valueOf("vertex missing"));
        }
        if (!(Boolean)contains_edge(graph, s, d)) {
            throw new RuntimeException(String.valueOf("edge missing"));
        }
        java.util.Map<String,String[]> adj_6 = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>()));
        for (String k : graph.adj.keySet()) {
adj_6.put(k, (String[])(((String[])(graph.adj).get(k))));
        }
adj_6.put(s, ((String[])(remove_from_list((String[])(((String[])(adj_6).get(s))), d))));
        if (!graph.directed) {
adj_6.put(d, ((String[])(remove_from_list((String[])(((String[])(adj_6).get(d))), s))));
        }
        return new Graph(adj_6, graph.directed);
    }

    static Graph remove_vertex(Graph graph, String v) {
        if (!(graph.adj.containsKey(v))) {
            throw new RuntimeException(String.valueOf("vertex missing"));
        }
        java.util.Map<String,String[]> adj_8 = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>()));
        for (String k : graph.adj.keySet()) {
            if (!(k.equals(v))) {
adj_8.put(k, ((String[])(remove_from_list((String[])(((String[])(graph.adj).get(k))), v))));
            }
        }
        return new Graph(adj_8, graph.directed);
    }

    static boolean contains_vertex(Graph graph, String v) {
        return graph.adj.containsKey(v);
    }

    static boolean contains_edge(Graph graph, String s, String d) {
        if (((!(graph.adj.containsKey(s))) || (!(graph.adj.containsKey(d))))) {
            throw new RuntimeException(String.valueOf("vertex missing"));
        }
        for (String x : ((String[])(graph.adj).get(s))) {
            if ((x.equals(d))) {
                return true;
            }
        }
        return false;
    }

    static Graph clear_graph(Graph graph) {
        return new Graph(new java.util.LinkedHashMap<String, String[]>(), graph.directed);
    }

    static String to_string(Graph graph) {
        return _p(graph.adj);
    }

    static void main() {
        String[] vertices = ((String[])(new String[]{"1", "2", "3", "4"}));
        String[][] edges_1 = ((String[][])(new String[][]{new String[]{"1", "2"}, new String[]{"2", "3"}, new String[]{"3", "4"}}));
        Graph g_1 = create_graph(((String[])(vertices)), ((String[][])(edges_1)), false);
        System.out.println(to_string(g_1));
        g_1 = add_vertex(g_1, "5");
        g_1 = add_edge(g_1, "4", "5");
        System.out.println(_p(contains_edge(g_1, "4", "5")));
        g_1 = remove_edge(g_1, "1", "2");
        g_1 = remove_vertex(g_1, "3");
        System.out.println(to_string(g_1));
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
