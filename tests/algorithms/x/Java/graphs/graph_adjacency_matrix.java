public class Main {
    static class Graph {
        boolean directed;
        java.util.Map<Long,Long> vertex_to_index;
        long[][] adj_matrix;
        Graph(boolean directed, java.util.Map<Long,Long> vertex_to_index, long[][] adj_matrix) {
            this.directed = directed;
            this.vertex_to_index = vertex_to_index;
            this.adj_matrix = adj_matrix;
        }
        Graph() {}
        @Override public String toString() {
            return String.format("{'directed': %s, 'vertex_to_index': %s, 'adj_matrix': %s}", String.valueOf(directed), String.valueOf(vertex_to_index), String.valueOf(adj_matrix));
        }
    }

    static Graph g_1 = null;

    static Graph make_graph(long[] vertices, long[][] edges, boolean directed) {
        Graph g = new Graph(directed, new java.util.LinkedHashMap<Long, Long>(), new long[][]{});
        long i_1 = 0L;
        while ((long)(i_1) < (long)(vertices.length)) {
            add_vertex(g, (long)(vertices[(int)((long)(i_1))]));
            i_1 = (long)((long)(i_1) + 1L);
        }
        long j_1 = 0L;
        while ((long)(j_1) < (long)(edges.length)) {
            long[] e_1 = ((long[])(edges[(int)((long)(j_1))]));
            add_edge(g, (long)(e_1[(int)((long)(0))]), (long)(e_1[(int)((long)(1))]));
            j_1 = (long)((long)(j_1) + 1L);
        }
        return g;
    }

    static boolean contains_vertex(Graph g, long v) {
        return g.vertex_to_index.containsKey(v);
    }

    static void add_vertex(Graph g, long v) {
        if (contains_vertex(g, (long)(v))) {
            throw new RuntimeException(String.valueOf("vertex already exists"));
        }
        long[][] matrix_1 = ((long[][])(g.adj_matrix));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(matrix_1.length)) {
matrix_1[(int)((long)(i_3))] = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(matrix_1[(int)((long)(i_3))]), java.util.stream.LongStream.of(0L)).toArray()));
            i_3 = (long)((long)(i_3) + 1L);
        }
        long[] row_1 = ((long[])(new long[]{}));
        long j_3 = 0L;
        while ((long)(j_3) < (long)((long)(matrix_1.length) + 1L)) {
            row_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(row_1), java.util.stream.LongStream.of(0L)).toArray()));
            j_3 = (long)((long)(j_3) + 1L);
        }
        matrix_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(matrix_1), java.util.stream.Stream.of(new long[][]{row_1})).toArray(long[][]::new)));
g.adj_matrix = matrix_1;
        java.util.Map<Long,Long> idx_map_1 = g.vertex_to_index;
idx_map_1.put(v, (long)((long)(matrix_1.length) - 1L));
g.vertex_to_index = idx_map_1;
    }

    static java.util.Map<Long,Long> remove_key(java.util.Map<Long,Long> m, long k) {
        java.util.Map<Long,Long> out = ((java.util.Map<Long,Long>)(new java.util.LinkedHashMap<Long, Long>()));
        for (long key : m.keySet()) {
            if ((long)(key) != (long)(k)) {
out.put(key, (long)(((long)(m).getOrDefault(key, 0L))));
            }
        }
        return out;
    }

    static java.util.Map<Long,Long> decrement_indices(java.util.Map<Long,Long> m, long start) {
        java.util.Map<Long,Long> out_1 = ((java.util.Map<Long,Long>)(new java.util.LinkedHashMap<Long, Long>()));
        for (long key : m.keySet()) {
            long idx_1 = (long)(((long)(m).getOrDefault(key, 0L)));
            if ((long)(idx_1) > (long)(start)) {
out_1.put(key, (long)((long)(idx_1) - 1L));
            } else {
out_1.put(key, (long)(idx_1));
            }
        }
        return out_1;
    }

    static void remove_vertex(Graph g, long v) {
        if (!(Boolean)contains_vertex(g, (long)(v))) {
            throw new RuntimeException(String.valueOf("vertex does not exist"));
        }
        long idx_3 = (long)(((long)((g.vertex_to_index)).getOrDefault(v, 0L)));
        long[][] new_matrix_1 = ((long[][])(new long[][]{}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(g.adj_matrix.length)) {
            if ((long)(i_5) != (long)(idx_3)) {
                long[] row_3 = ((long[])(g.adj_matrix[(int)((long)(i_5))]));
                long[] new_row_1 = ((long[])(new long[]{}));
                long j_5 = 0L;
                while ((long)(j_5) < (long)(row_3.length)) {
                    if ((long)(j_5) != (long)(idx_3)) {
                        new_row_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(new_row_1), java.util.stream.LongStream.of((long)(row_3[(int)((long)(j_5))]))).toArray()));
                    }
                    j_5 = (long)((long)(j_5) + 1L);
                }
                new_matrix_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_matrix_1), java.util.stream.Stream.of(new long[][]{new_row_1})).toArray(long[][]::new)));
            }
            i_5 = (long)((long)(i_5) + 1L);
        }
g.adj_matrix = new_matrix_1;
        java.util.Map<Long,Long> m_1 = remove_key(g.vertex_to_index, (long)(v));
g.vertex_to_index = decrement_indices(m_1, (long)(idx_3));
    }

    static void add_edge(Graph g, long u, long v) {
        if (!(contains_vertex(g, (long)(u)) && contains_vertex(g, (long)(v)))) {
            throw new RuntimeException(String.valueOf("missing vertex"));
        }
        long i_7 = (long)(((long)((g.vertex_to_index)).getOrDefault(u, 0L)));
        long j_7 = (long)(((long)((g.vertex_to_index)).getOrDefault(v, 0L)));
        long[][] matrix_3 = ((long[][])(g.adj_matrix));
matrix_3[(int)((long)(i_7))][(int)((long)(j_7))] = 1L;
        if (!g.directed) {
matrix_3[(int)((long)(j_7))][(int)((long)(i_7))] = 1L;
        }
g.adj_matrix = matrix_3;
    }

    static void remove_edge(Graph g, long u, long v) {
        if (!(contains_vertex(g, (long)(u)) && contains_vertex(g, (long)(v)))) {
            throw new RuntimeException(String.valueOf("missing vertex"));
        }
        long i_9 = (long)(((long)((g.vertex_to_index)).getOrDefault(u, 0L)));
        long j_9 = (long)(((long)((g.vertex_to_index)).getOrDefault(v, 0L)));
        long[][] matrix_5 = ((long[][])(g.adj_matrix));
matrix_5[(int)((long)(i_9))][(int)((long)(j_9))] = 0L;
        if (!g.directed) {
matrix_5[(int)((long)(j_9))][(int)((long)(i_9))] = 0L;
        }
g.adj_matrix = matrix_5;
    }

    static boolean contains_edge(Graph g, long u, long v) {
        if (!(contains_vertex(g, (long)(u)) && contains_vertex(g, (long)(v)))) {
            throw new RuntimeException(String.valueOf("missing vertex"));
        }
        long i_11 = (long)(((long)((g.vertex_to_index)).getOrDefault(u, 0L)));
        long j_11 = (long)(((long)((g.vertex_to_index)).getOrDefault(v, 0L)));
        long[][] matrix_7 = ((long[][])(g.adj_matrix));
        return (long)(matrix_7[(int)((long)(i_11))][(int)((long)(j_11))]) == 1L;
    }

    static void clear_graph(Graph g) {
g.vertex_to_index = new java.util.LinkedHashMap<String, Object>();
g.adj_matrix = new long[][]{};
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            g_1 = make_graph(((long[])(new long[]{1, 2, 3})), ((long[][])(new long[][]{new long[]{1, 2}, new long[]{2, 3}})), false);
            System.out.println(_p(g_1.adj_matrix));
            System.out.println(_p(contains_edge(g_1, 1L, 2L)));
            System.out.println(_p(contains_edge(g_1, 2L, 1L)));
            remove_edge(g_1, 1L, 2L);
            System.out.println(_p(contains_edge(g_1, 1L, 2L)));
            remove_vertex(g_1, 2L);
            System.out.println(_p(g_1.adj_matrix));
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
