public class Main {
    static class Edge {
        int destination_vertex;
        int weight;
        Edge(int destination_vertex, int weight) {
            this.destination_vertex = destination_vertex;
            this.weight = weight;
        }
        Edge() {}
        @Override public String toString() {
            return String.format("{'destination_vertex': %s, 'weight': %s}", String.valueOf(destination_vertex), String.valueOf(weight));
        }
    }

    static class AdjacencyList {
        Edge[][] graph;
        int size;
        AdjacencyList(Edge[][] graph, int size) {
            this.graph = graph;
            this.size = size;
        }
        AdjacencyList() {}
        @Override public String toString() {
            return String.format("{'graph': %s, 'size': %s}", String.valueOf(graph), String.valueOf(size));
        }
    }

    static AdjacencyList g_2;

    static AdjacencyList new_adjacency_list(int size) {
        Edge[][] g = ((Edge[][])(new Edge[][]{}));
        int i = 0;
        while (i < size) {
            g = ((Edge[][])(appendObj(g, new Edge[]{})));
            i = i + 1;
        }
        return new AdjacencyList(g, size);
    }

    static void add_edge(AdjacencyList al, int from_vertex, int to_vertex, int weight) {
        if (!(weight == 0 || weight == 1)) {
            throw new RuntimeException(String.valueOf("Edge weight must be either 0 or 1."));
        }
        if (to_vertex < 0 || to_vertex >= al.size) {
            throw new RuntimeException(String.valueOf("Vertex indexes must be in [0; size)."));
        }
        Edge[][] g_1 = ((Edge[][])(al.graph));
        Edge[] edges = ((Edge[])(g_1[from_vertex]));
g_1[from_vertex] = ((Edge[])(java.util.stream.Stream.concat(java.util.Arrays.stream(edges), java.util.stream.Stream.of(new Edge(to_vertex, weight))).toArray(Edge[]::new)));
al.graph = g_1;
    }

    static int[] push_front(int[] q, int v) {
        int[] res = ((int[])(new int[]{v}));
        int i_1 = 0;
        while (i_1 < q.length) {
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(q[i_1])).toArray()));
            i_1 = i_1 + 1;
        }
        return res;
    }

    static int[] pop_front(int[] q) {
        int[] res_1 = ((int[])(new int[]{}));
        int i_2 = 1;
        while (i_2 < q.length) {
            res_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_1), java.util.stream.IntStream.of(q[i_2])).toArray()));
            i_2 = i_2 + 1;
        }
        return res_1;
    }

    static int front(int[] q) {
        return q[0];
    }

    static int get_shortest_path(AdjacencyList al, int start_vertex, int finish_vertex) {
        int[] queue = ((int[])(new int[]{start_vertex}));
        int[] distances = ((int[])(new int[]{}));
        int i_3 = 0;
        while (i_3 < al.size) {
            distances = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(distances), java.util.stream.IntStream.of(-1)).toArray()));
            i_3 = i_3 + 1;
        }
distances[start_vertex] = 0;
        while (queue.length > 0) {
            int current_vertex = front(((int[])(queue)));
            queue = ((int[])(pop_front(((int[])(queue)))));
            int current_distance = distances[current_vertex];
            Edge[] edges_1 = ((Edge[])(al.graph[current_vertex]));
            int j = 0;
            while (j < edges_1.length) {
                Edge edge = edges_1[j];
                int new_distance = current_distance + edge.weight;
                int dest = edge.destination_vertex;
                int dest_distance = distances[dest];
                if (dest_distance >= 0 && new_distance >= dest_distance) {
                    j = j + 1;
                    continue;
                }
distances[dest] = new_distance;
                if (edge.weight == 0) {
                    queue = ((int[])(push_front(((int[])(queue)), dest)));
                } else {
                    queue = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(queue), java.util.stream.IntStream.of(dest)).toArray()));
                }
                j = j + 1;
            }
        }
        int result = distances[finish_vertex];
        if (result < 0) {
            throw new RuntimeException(String.valueOf("No path from start_vertex to finish_vertex."));
        }
        return result;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            g_2 = new_adjacency_list(11);
            add_edge(g_2, 0, 1, 0);
            add_edge(g_2, 0, 3, 1);
            add_edge(g_2, 1, 2, 0);
            add_edge(g_2, 2, 3, 0);
            add_edge(g_2, 4, 2, 1);
            add_edge(g_2, 4, 5, 1);
            add_edge(g_2, 4, 6, 1);
            add_edge(g_2, 5, 9, 0);
            add_edge(g_2, 6, 7, 1);
            add_edge(g_2, 7, 8, 1);
            add_edge(g_2, 8, 10, 1);
            add_edge(g_2, 9, 7, 0);
            add_edge(g_2, 9, 10, 1);
            System.out.println(_p(get_shortest_path(g_2, 0, 3)));
            System.out.println(_p(get_shortest_path(g_2, 4, 10)));
            System.out.println(_p(get_shortest_path(g_2, 4, 8)));
            System.out.println(_p(get_shortest_path(g_2, 0, 1)));
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
