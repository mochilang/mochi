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
            String s = e[0];
            String d = e[1];
            if (!(Boolean)(adj.containsKey(s))) {
adj.put(s, ((String[])(new String[]{})));
            }
            if (!(Boolean)(adj.containsKey(d))) {
adj.put(d, ((String[])(new String[]{})));
            }
adj.put(s, ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(((String[])(adj).get(s))), java.util.stream.Stream.of(d)).toArray(String[]::new))));
            if (!(Boolean)directed) {
adj.put(d, ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(((String[])(adj).get(d))), java.util.stream.Stream.of(s)).toArray(String[]::new))));
            }
        }
        return new Graph(adj, directed);
    }

    static Graph add_vertex(Graph graph, String v) {
        if (((Boolean)(graph.adj.containsKey(v)))) {
            throw new RuntimeException(String.valueOf("vertex exists"));
        }
        java.util.Map<String,String[]> adj_1 = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>()));
        for (String k : graph.adj.keySet()) {
adj_1.put(k, (String[])(((String[])(graph.adj).get(k))));
        }
adj_1.put(v, ((String[])(new String[]{})));
        return new Graph(adj_1, graph.directed);
    }

    static String[] remove_from_list(String[] lst, String value) {
        String[] res = ((String[])(new String[]{}));
        int i = 0;
        while (i < lst.length) {
            if (!(lst[i].equals(value))) {
                res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(lst[i])).toArray(String[]::new)));
            }
            i = i + 1;
        }
        return res;
    }

    static java.util.Map<String,String[]> remove_key(java.util.Map<String,String[]> m, String key) {
        java.util.Map<String,String[]> res_1 = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>()));
        for (String k : m.keySet()) {
            if (((Number)(k)).intValue() != key) {
res_1.put(k, (String[])(((String[])(m).get(k))));
            }
        }
        return res_1;
    }

    static Graph add_edge(Graph graph, String s, String d) {
        if (((!(Boolean)(graph.adj.containsKey(s))) || (!(Boolean)(graph.adj.containsKey(d))))) {
            throw new RuntimeException(String.valueOf("vertex missing"));
        }
        if (((Boolean)(contains_edge(graph, s, d)))) {
            throw new RuntimeException(String.valueOf("edge exists"));
        }
        java.util.Map<String,String[]> adj_2 = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>()));
        for (String k : graph.adj.keySet()) {
adj_2.put(k, (String[])(((String[])(graph.adj).get(k))));
        }
        String[] list_s = (String[])(((String[])(adj_2).get(s)));
        list_s = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(list_s), java.util.stream.Stream.of(d)).toArray(String[]::new)));
adj_2.put(s, ((String[])(list_s)));
        if (!graph.directed) {
            String[] list_d = (String[])(((String[])(adj_2).get(d)));
            list_d = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(list_d), java.util.stream.Stream.of(s)).toArray(String[]::new)));
adj_2.put(d, ((String[])(list_d)));
        }
        return new Graph(adj_2, graph.directed);
    }

    static Graph remove_edge(Graph graph, String s, String d) {
        if (((!(Boolean)(graph.adj.containsKey(s))) || (!(Boolean)(graph.adj.containsKey(d))))) {
            throw new RuntimeException(String.valueOf("vertex missing"));
        }
        if (!(Boolean)contains_edge(graph, s, d)) {
            throw new RuntimeException(String.valueOf("edge missing"));
        }
        java.util.Map<String,String[]> adj_3 = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>()));
        for (String k : graph.adj.keySet()) {
adj_3.put(k, (String[])(((String[])(graph.adj).get(k))));
        }
adj_3.put(s, ((String[])(remove_from_list((String[])(((String[])(adj_3).get(s))), d))));
        if (!graph.directed) {
adj_3.put(d, ((String[])(remove_from_list((String[])(((String[])(adj_3).get(d))), s))));
        }
        return new Graph(adj_3, graph.directed);
    }

    static Graph remove_vertex(Graph graph, String v) {
        if (!(Boolean)(graph.adj.containsKey(v))) {
            throw new RuntimeException(String.valueOf("vertex missing"));
        }
        java.util.Map<String,String[]> adj_4 = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>()));
        for (String k : graph.adj.keySet()) {
            if (((Number)(k)).intValue() != v) {
adj_4.put(k, ((String[])(remove_from_list((String[])(((String[])(graph.adj).get(k))), v))));
            }
        }
        return new Graph(adj_4, graph.directed);
    }

    static boolean contains_vertex(Graph graph, String v) {
        return graph.adj.containsKey(v);
    }

    static boolean contains_edge(Graph graph, String s, String d) {
        if (((!(Boolean)(graph.adj.containsKey(s))) || (!(Boolean)(graph.adj.containsKey(d))))) {
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
        String[][] edges = ((String[][])(new String[][]{new String[]{"1", "2"}, new String[]{"2", "3"}, new String[]{"3", "4"}}));
        Graph g = create_graph(((String[])(vertices)), ((String[][])(edges)), false);
        System.out.println(to_string(g));
        g = add_vertex(g, "5");
        g = add_edge(g, "4", "5");
        System.out.println(_p(contains_edge(g, "4", "5")));
        g = remove_edge(g, "1", "2");
        g = remove_vertex(g, "3");
        System.out.println(to_string(g));
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
        return String.valueOf(v);
    }
}
