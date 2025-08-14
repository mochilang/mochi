public class Main {
    static java.util.Map<String,String[]> demo_graph;

    static boolean contains(String[] xs, String x) {
        long i = 0L;
        while ((long)(i) < (long)(xs.length)) {
            if ((xs[(int)((long)(i))].equals(x))) {
                return true;
            }
            i = (long)((long)(i) + 1L);
        }
        return false;
    }

    static boolean contains_key(java.util.Map<String,String[]> m, String key) {
        for (String k : m.keySet()) {
            if ((k.equals(key))) {
                return true;
            }
        }
        return false;
    }

    static String[] bfs_shortest_path(java.util.Map<String,String[]> graph, String start, String goal) {
        String[] explored = ((String[])(new String[]{}));
        String[][] queue_1 = ((String[][])(new String[][]{new String[]{start}}));
        if ((start.equals(goal))) {
            return new String[]{start};
        }
        while ((long)(queue_1.length) > 0L) {
            String[] path_1 = ((String[])(queue_1[(int)((long)(0))]));
            queue_1 = ((String[][])(java.util.Arrays.copyOfRange(queue_1, (int)((long)(1)), (int)((long)(queue_1.length)))));
            String node_1 = path_1[(int)((long)((long)(path_1.length) - 1L))];
            if (!(Boolean)contains(((String[])(explored)), node_1)) {
                String[] neighbours_1 = (String[])(((String[])(graph).get(node_1)));
                long i_2 = 0L;
                while ((long)(i_2) < (long)(neighbours_1.length)) {
                    String neighbour_1 = neighbours_1[(int)((long)(i_2))];
                    String[] new_path_1 = ((String[])(path_1));
                    new_path_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_path_1), java.util.stream.Stream.of(neighbour_1)).toArray(String[]::new)));
                    queue_1 = ((String[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_1), java.util.stream.Stream.of(new String[][]{new_path_1})).toArray(String[][]::new)));
                    if ((neighbour_1.equals(goal))) {
                        return new_path_1;
                    }
                    i_2 = (long)((long)(i_2) + 1L);
                }
                explored = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(explored), java.util.stream.Stream.of(node_1)).toArray(String[]::new)));
            }
        }
        return new String[]{};
    }

    static long bfs_shortest_path_distance(java.util.Map<String,String[]> graph, String start, String target) {
        if (((contains_key(graph, start) == false)) || ((contains_key(graph, target) == false))) {
            return -1;
        }
        if ((start.equals(target))) {
            return 0;
        }
        String[] queue_3 = ((String[])(new String[]{start}));
        String[] visited_1 = ((String[])(new String[]{start}));
        java.util.Map<String,Long> dist_1 = ((java.util.Map<String,Long>)(new java.util.LinkedHashMap<String, Long>()));
dist_1.put(start, 0L);
dist_1.put(target, (long)((-1)));
        while ((long)(queue_3.length) > 0L) {
            String node_3 = queue_3[(int)((long)(0))];
            queue_3 = ((String[])(java.util.Arrays.copyOfRange(queue_3, (int)((long)(1)), (int)((long)(queue_3.length)))));
            if ((node_3.equals(target))) {
                if ((long)(((long)(dist_1).getOrDefault(target, 0L))) == (long)((-1)) || (long)(((long)(dist_1).getOrDefault(node_3, 0L))) < (long)(((long)(dist_1).getOrDefault(target, 0L)))) {
dist_1.put(target, (long)(((long)(dist_1).getOrDefault(node_3, 0L))));
                }
            }
            String[] adj_1 = (String[])(((String[])(graph).get(node_3)));
            long i_4 = 0L;
            while ((long)(i_4) < (long)(adj_1.length)) {
                String next_1 = adj_1[(int)((long)(i_4))];
                if (!(Boolean)contains(((String[])(visited_1)), next_1)) {
                    visited_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(visited_1), java.util.stream.Stream.of(next_1)).toArray(String[]::new)));
                    queue_3 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_3), java.util.stream.Stream.of(next_1)).toArray(String[]::new)));
dist_1.put(next_1, (long)((long)(((long)(dist_1).getOrDefault(node_3, 0L))) + 1L));
                }
                i_4 = (long)((long)(i_4) + 1L);
            }
        }
        return ((long)(dist_1).getOrDefault(target, 0L));
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
}
