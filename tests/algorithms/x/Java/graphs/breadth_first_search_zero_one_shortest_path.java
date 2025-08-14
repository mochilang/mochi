public class Main {
    static class Edge {
        long destination_vertex;
        long weight;
        Edge(long destination_vertex, long weight) {
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
        long size;
        AdjacencyList(Edge[][] graph, long size) {
            this.graph = graph;
            this.size = size;
        }
        AdjacencyList() {}
        @Override public String toString() {
            return String.format("{'graph': %s, 'size': %s}", String.valueOf(graph), String.valueOf(size));
        }
    }

    static AdjacencyList g_3;

    static AdjacencyList new_adjacency_list(long size) {
        Edge[][] g = ((Edge[][])(new Edge[][]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(size)) {
            g = ((Edge[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(g), java.util.stream.Stream.of(new Edge[][]{new Edge[]{}})).toArray(Edge[][]::new)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return new AdjacencyList(g, size);
    }

    static void add_edge(AdjacencyList al, long from_vertex, long to_vertex, long weight) {
        if (!((long)(weight) == 0L || (long)(weight) == 1L)) {
            throw new RuntimeException(String.valueOf("Edge weight must be either 0 or 1."));
        }
        if ((long)(to_vertex) < 0L || (long)(to_vertex) >= (long)(al.size)) {
            throw new RuntimeException(String.valueOf("Vertex indexes must be in [0; size)."));
        }
        Edge[][] g_2 = ((Edge[][])(al.graph));
        Edge[] edges_1 = ((Edge[])(g_2[(int)((long)(from_vertex))]));
g_2[(int)((long)(from_vertex))] = ((Edge[])(java.util.stream.Stream.concat(java.util.Arrays.stream(edges_1), java.util.stream.Stream.of(new Edge(to_vertex, weight))).toArray(Edge[]::new)));
al.graph = g_2;
    }

    static long[] push_front(long[] q, long v) {
        long[] res = ((long[])(new long[]{v}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(q.length)) {
            res = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res), java.util.stream.LongStream.of((long)(q[(int)((long)(i_3))]))).toArray()));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return res;
    }

    static long[] pop_front(long[] q) {
        long[] res_1 = ((long[])(new long[]{}));
        long i_5 = 1L;
        while ((long)(i_5) < (long)(q.length)) {
            res_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res_1), java.util.stream.LongStream.of((long)(q[(int)((long)(i_5))]))).toArray()));
            i_5 = (long)((long)(i_5) + 1L);
        }
        return res_1;
    }

    static long front(long[] q) {
        return q[(int)((long)(0))];
    }

    static long get_shortest_path(AdjacencyList al, long start_vertex, long finish_vertex) {
        long[] queue = ((long[])(new long[]{start_vertex}));
        long[] distances_1 = ((long[])(new long[]{}));
        long i_7 = 0L;
        while ((long)(i_7) < (long)(al.size)) {
            distances_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(distances_1), java.util.stream.LongStream.of((long)(-1))).toArray()));
            i_7 = (long)((long)(i_7) + 1L);
        }
distances_1[(int)((long)(start_vertex))] = 0L;
        while ((long)(queue.length) > 0L) {
            long current_vertex_1 = (long)(front(((long[])(queue))));
            queue = ((long[])(pop_front(((long[])(queue)))));
            long current_distance_1 = (long)(distances_1[(int)((long)(current_vertex_1))]);
            Edge[] edges_3 = ((Edge[])(al.graph[(int)((long)(current_vertex_1))]));
            long j_1 = 0L;
            while ((long)(j_1) < (long)(edges_3.length)) {
                Edge edge_1 = edges_3[(int)((long)(j_1))];
                long new_distance_1 = (long)((long)(current_distance_1) + (long)(edge_1.weight));
                long dest_1 = (long)(edge_1.destination_vertex);
                long dest_distance_1 = (long)(distances_1[(int)((long)(dest_1))]);
                if ((long)(dest_distance_1) >= 0L && (long)(new_distance_1) >= (long)(dest_distance_1)) {
                    j_1 = (long)((long)(j_1) + 1L);
                    continue;
                }
distances_1[(int)((long)(dest_1))] = (long)(new_distance_1);
                if ((long)(edge_1.weight) == 0L) {
                    queue = ((long[])(push_front(((long[])(queue)), (long)(dest_1))));
                } else {
                    queue = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(queue), java.util.stream.LongStream.of((long)(dest_1))).toArray()));
                }
                j_1 = (long)((long)(j_1) + 1L);
            }
        }
        long result_1 = (long)(distances_1[(int)((long)(finish_vertex))]);
        if ((long)(result_1) < 0L) {
            throw new RuntimeException(String.valueOf("No path from start_vertex to finish_vertex."));
        }
        return result_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            g_3 = new_adjacency_list(11L);
            add_edge(g_3, 0L, 1L, 0L);
            add_edge(g_3, 0L, 3L, 1L);
            add_edge(g_3, 1L, 2L, 0L);
            add_edge(g_3, 2L, 3L, 0L);
            add_edge(g_3, 4L, 2L, 1L);
            add_edge(g_3, 4L, 5L, 1L);
            add_edge(g_3, 4L, 6L, 1L);
            add_edge(g_3, 5L, 9L, 0L);
            add_edge(g_3, 6L, 7L, 1L);
            add_edge(g_3, 7L, 8L, 1L);
            add_edge(g_3, 8L, 10L, 1L);
            add_edge(g_3, 9L, 7L, 0L);
            add_edge(g_3, 9L, 10L, 1L);
            System.out.println(_p(get_shortest_path(g_3, 0L, 3L)));
            System.out.println(_p(get_shortest_path(g_3, 4L, 10L)));
            System.out.println(_p(get_shortest_path(g_3, 4L, 8L)));
            System.out.println(_p(get_shortest_path(g_3, 0L, 1L)));
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
