public class Main {
    static class Graph {
        java.util.Map<String,String[]> graph;
        java.util.Map<String,String> parent;
        String source;
        Graph(java.util.Map<String,String[]> graph, java.util.Map<String,String> parent, String source) {
            this.graph = graph;
            this.parent = parent;
            this.source = source;
        }
        Graph() {}
        @Override public String toString() {
            return String.format("{'graph': %s, 'parent': %s, 'source': '%s'}", String.valueOf(graph), String.valueOf(parent), String.valueOf(source));
        }
    }

    static java.util.Map<String,String[]> graph;
    static Graph g = null;

    static Graph newGraph(java.util.Map<String,String[]> g, String s) {
        return new Graph(g, new java.util.LinkedHashMap<String, String>(), s);
    }

    static Graph breath_first_search(Graph g) {
        java.util.Map<String,String> parent = g.parent;
parent.put(g.source, g.source);
        String[] queue = ((String[])(new String[]{g.source}));
        int idx = 0;
        while (idx < queue.length) {
            String vertex = queue[idx];
            for (String adj : ((String[])(g.graph).get(vertex))) {
                if (!(Boolean)(parent.containsKey(adj))) {
parent.put(adj, vertex);
                    queue = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue), java.util.stream.Stream.of(adj)).toArray(String[]::new)));
                }
            }
            idx = idx + 1;
        }
g.parent = parent;
        return g;
    }

    static String shortest_path(Graph g, String target) {
        if ((target.equals(g.source))) {
            return g.source;
        }
        if (!(Boolean)(g.parent.containsKey(target))) {
            return "No path from vertex: " + g.source + " to vertex: " + target;
        }
        String p = ((String)(g.parent).get(target));
        return String.valueOf(shortest_path(g, p)) + "->" + target;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            graph = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>(java.util.Map.ofEntries(java.util.Map.entry("A", ((String[])(new String[]{"B", "C", "E"}))), java.util.Map.entry("B", ((String[])(new String[]{"A", "D", "E"}))), java.util.Map.entry("C", ((String[])(new String[]{"A", "F", "G"}))), java.util.Map.entry("D", ((String[])(new String[]{"B"}))), java.util.Map.entry("E", ((String[])(new String[]{"A", "B", "D"}))), java.util.Map.entry("F", ((String[])(new String[]{"C"}))), java.util.Map.entry("G", ((String[])(new String[]{"C"})))))));
            g = newGraph(graph, "G");
            g = breath_first_search(g);
            System.out.println(shortest_path(g, "D"));
            System.out.println(shortest_path(g, "G"));
            System.out.println(shortest_path(g, "Foo"));
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
}
