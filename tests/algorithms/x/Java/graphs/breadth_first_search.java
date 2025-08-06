public class Main {
    static java.util.Map<Integer,int[]> g = null;

    static void add_edge(java.util.Map<Integer,int[]> graph, int from, int to) {
        if (((Boolean)(graph.containsKey(from)))) {
graph.put(from, ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(((int[])(graph).get(from))), java.util.stream.IntStream.of(to)).toArray())));
        } else {
graph.put(from, ((int[])(new int[]{to})));
        }
    }

    static void print_graph(java.util.Map<Integer,int[]> graph) {
        for (var v : new java.util.ArrayList<>(graph.keySet())) {
            int[] adj = (int[])(((int[])(graph).get(v)));
            String line = _p(v) + "  :  ";
            int i = 0;
            while (i < adj.length) {
                line = line + _p(_geti(adj, i));
                if (i < adj.length - 1) {
                    line = line + " -> ";
                }
                i = i + 1;
            }
            System.out.println(line);
        }
    }

    static int[] bfs(java.util.Map<Integer,int[]> graph, int start) {
        java.util.Map<Integer,Boolean> visited = ((java.util.Map<Integer,Boolean>)(new java.util.LinkedHashMap<Integer, Boolean>()));
        int[] queue = ((int[])(new int[]{}));
        int[] order = ((int[])(new int[]{}));
        queue = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(queue), java.util.stream.IntStream.of(start)).toArray()));
visited.put(start, true);
        int head = 0;
        while (head < queue.length) {
            int vertex = queue[head];
            head = head + 1;
            order = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(order), java.util.stream.IntStream.of(vertex)).toArray()));
            int[] neighbors = (int[])(((int[])(graph).get(vertex)));
            int i_1 = 0;
            while (i_1 < neighbors.length) {
                int neighbor = neighbors[i_1];
                if (!(Boolean)(visited.containsKey(neighbor))) {
visited.put(neighbor, true);
                    queue = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(queue), java.util.stream.IntStream.of(neighbor)).toArray()));
                }
                i_1 = i_1 + 1;
            }
        }
        return order;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            g = ((java.util.Map<Integer,int[]>)(new java.util.LinkedHashMap<Integer, int[]>()));
            add_edge(g, 0, 1);
            add_edge(g, 0, 2);
            add_edge(g, 1, 2);
            add_edge(g, 2, 0);
            add_edge(g, 2, 3);
            add_edge(g, 3, 3);
            print_graph(g);
            System.out.println(bfs(g, 2));
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
