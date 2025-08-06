public class Main {
    static class GraphAdjacencyList {
        java.util.Map<String,String[]> adj_list;
        boolean directed;
        GraphAdjacencyList(java.util.Map<String,String[]> adj_list, boolean directed) {
            this.adj_list = adj_list;
            this.directed = directed;
        }
        GraphAdjacencyList() {}
        @Override public String toString() {
            return String.format("{'adj_list': %s, 'directed': %s}", String.valueOf(adj_list), String.valueOf(directed));
        }
    }

    static GraphAdjacencyList d_graph = null;

    static GraphAdjacencyList make_graph(boolean directed) {
        java.util.Map<String,String[]> m = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>()));
        return new GraphAdjacencyList(m, directed);
    }

    static boolean contains_vertex(java.util.Map<String,String[]> m, String v) {
        return m.containsKey(v);
    }

    static GraphAdjacencyList add_edge(GraphAdjacencyList g, String s, String d) {
        java.util.Map<String,String[]> adj = g.adj_list;
        if (!g.directed) {
            if (((Boolean)(contains_vertex(adj, s))) && ((Boolean)(contains_vertex(adj, d)))) {
adj.put(s, ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(((String[])(adj).get(s))), java.util.stream.Stream.of(d)).toArray(String[]::new))));
adj.put(d, ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(((String[])(adj).get(d))), java.util.stream.Stream.of(s)).toArray(String[]::new))));
            } else             if (((Boolean)(contains_vertex(adj, s)))) {
adj.put(s, ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(((String[])(adj).get(s))), java.util.stream.Stream.of(d)).toArray(String[]::new))));
adj.put(d, ((String[])(new String[]{s})));
            } else             if (((Boolean)(contains_vertex(adj, d)))) {
adj.put(d, ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(((String[])(adj).get(d))), java.util.stream.Stream.of(s)).toArray(String[]::new))));
adj.put(s, ((String[])(new String[]{d})));
            } else {
adj.put(s, ((String[])(new String[]{d})));
adj.put(d, ((String[])(new String[]{s})));
            }
        } else         if (((Boolean)(contains_vertex(adj, s))) && ((Boolean)(contains_vertex(adj, d)))) {
adj.put(s, ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(((String[])(adj).get(s))), java.util.stream.Stream.of(d)).toArray(String[]::new))));
        } else         if (((Boolean)(contains_vertex(adj, s)))) {
adj.put(s, ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(((String[])(adj).get(s))), java.util.stream.Stream.of(d)).toArray(String[]::new))));
adj.put(d, ((String[])(new String[]{})));
        } else         if (((Boolean)(contains_vertex(adj, d)))) {
adj.put(s, ((String[])(new String[]{d})));
        } else {
adj.put(s, ((String[])(new String[]{d})));
adj.put(d, ((String[])(new String[]{})));
        }
g.adj_list = adj;
        return g;
    }

    static String graph_to_string(GraphAdjacencyList g) {
        return _p(g.adj_list);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            d_graph = make_graph(true);
            d_graph = add_edge(d_graph, _p(0), _p(1));
            System.out.println(graph_to_string(d_graph));
            d_graph = add_edge(d_graph, _p(1), _p(2));
            d_graph = add_edge(d_graph, _p(1), _p(4));
            d_graph = add_edge(d_graph, _p(1), _p(5));
            System.out.println(graph_to_string(d_graph));
            d_graph = add_edge(d_graph, _p(2), _p(0));
            d_graph = add_edge(d_graph, _p(2), _p(6));
            d_graph = add_edge(d_graph, _p(2), _p(7));
            System.out.println(graph_to_string(d_graph));
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
