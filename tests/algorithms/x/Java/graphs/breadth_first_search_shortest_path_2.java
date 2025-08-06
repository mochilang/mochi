public class Main {
    static java.util.Map<String,String[]> demo_graph;

    static boolean contains(String[] xs, String x) {
        int i = 0;
        while (i < xs.length) {
            if ((xs[i].equals(x))) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static boolean contains_key(java.util.Map<String,String[]> m, String key) {
        for (String k : m.keySet()) {
            if (((Number)(k)).intValue() == key) {
                return true;
            }
        }
        return false;
    }

    static String[] bfs_shortest_path(java.util.Map<String,String[]> graph, String start, String goal) {
        String[] explored = ((String[])(new String[]{}));
        String[][] queue = ((String[][])(new String[][]{new String[]{start}}));
        if ((start.equals(goal))) {
            return new String[]{start};
        }
        while (queue.length > 0) {
            String[] path = ((String[])(queue[0]));
            queue = ((String[][])(java.util.Arrays.copyOfRange(queue, 1, queue.length)));
            String node = path[path.length - 1];
            if (!(Boolean)contains(((String[])(explored)), node)) {
                String[] neighbours = (String[])(((String[])(graph).get(node)));
                int i_1 = 0;
                while (i_1 < neighbours.length) {
                    String neighbour = neighbours[i_1];
                    String[] new_path = ((String[])(path));
                    new_path = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_path), java.util.stream.Stream.of(neighbour)).toArray(String[]::new)));
                    queue = ((String[][])(appendObj(queue, new_path)));
                    if ((neighbour.equals(goal))) {
                        return new_path;
                    }
                    i_1 = i_1 + 1;
                }
                explored = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(explored), java.util.stream.Stream.of(node)).toArray(String[]::new)));
            }
        }
        return new String[]{};
    }

    static int bfs_shortest_path_distance(java.util.Map<String,String[]> graph, String start, String target) {
        if ((contains_key(graph, start) == false) || (contains_key(graph, target) == false)) {
            return -1;
        }
        if ((start.equals(target))) {
            return 0;
        }
        String[] queue_1 = ((String[])(new String[]{start}));
        String[] visited = ((String[])(new String[]{start}));
        java.util.Map<String,Integer> dist = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
dist.put(start, 0);
dist.put(target, (-1));
        while (queue_1.length > 0) {
            String node_1 = queue_1[0];
            queue_1 = ((String[])(java.util.Arrays.copyOfRange(queue_1, 1, queue_1.length)));
            if ((node_1.equals(target))) {
                if ((int)(((int)(dist).getOrDefault(target, 0))) == (-1) || (int)(((int)(dist).getOrDefault(node_1, 0))) < (int)(((int)(dist).getOrDefault(target, 0)))) {
dist.put(target, (int)(((int)(dist).getOrDefault(node_1, 0))));
                }
            }
            String[] adj = (String[])(((String[])(graph).get(node_1)));
            int i_2 = 0;
            while (i_2 < adj.length) {
                String next = adj[i_2];
                if (!(Boolean)contains(((String[])(visited)), next)) {
                    visited = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(visited), java.util.stream.Stream.of(next)).toArray(String[]::new)));
                    queue_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_1), java.util.stream.Stream.of(next)).toArray(String[]::new)));
dist.put(next, (int)(((int)(dist).getOrDefault(node_1, 0))) + 1);
                }
                i_2 = i_2 + 1;
            }
        }
        return ((int)(dist).getOrDefault(target, 0));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            demo_graph = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>(java.util.Map.ofEntries(java.util.Map.entry("A", ((String[])(new String[]{"B", "C", "E"}))), java.util.Map.entry("B", ((String[])(new String[]{"A", "D", "E"}))), java.util.Map.entry("C", ((String[])(new String[]{"A", "F", "G"}))), java.util.Map.entry("D", ((String[])(new String[]{"B"}))), java.util.Map.entry("E", ((String[])(new String[]{"A", "B", "D"}))), java.util.Map.entry("F", ((String[])(new String[]{"C"}))), java.util.Map.entry("G", ((String[])(new String[]{"C"})))))));
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
}
