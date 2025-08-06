public class Main {
    static class Graph {
        boolean directed;
        java.util.Map<Integer,Integer> vertex_to_index;
        int[][] adj_matrix;
        Graph(boolean directed, java.util.Map<Integer,Integer> vertex_to_index, int[][] adj_matrix) {
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

    static Graph make_graph(int[] vertices, int[][] edges, boolean directed) {
        Graph g = new Graph(directed, new java.util.LinkedHashMap<Integer, Integer>(), new int[][]{});
        int i = 0;
        while (i < vertices.length) {
            add_vertex(g, vertices[i]);
            i = i + 1;
        }
        int j = 0;
        while (j < edges.length) {
            int[] e = ((int[])(edges[j]));
            add_edge(g, e[0], e[1]);
            j = j + 1;
        }
        return g;
    }

    static boolean contains_vertex(Graph g, int v) {
        return g.vertex_to_index.containsKey(v);
    }

    static void add_vertex(Graph g, int v) {
        if (((Boolean)(contains_vertex(g, v)))) {
            throw new RuntimeException(String.valueOf("vertex already exists"));
        }
        int[][] matrix = ((int[][])(g.adj_matrix));
        int i_1 = 0;
        while (i_1 < matrix.length) {
matrix[i_1] = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(matrix[i_1]), java.util.stream.IntStream.of(0)).toArray()));
            i_1 = i_1 + 1;
        }
        int[] row = ((int[])(new int[]{}));
        int j_1 = 0;
        while (j_1 < matrix.length + 1) {
            row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray()));
            j_1 = j_1 + 1;
        }
        matrix = ((int[][])(appendObj(matrix, row)));
g.adj_matrix = matrix;
        java.util.Map<Integer,Integer> idx_map = g.vertex_to_index;
idx_map.put(v, matrix.length - 1);
g.vertex_to_index = idx_map;
    }

    static java.util.Map<Integer,Integer> remove_key(java.util.Map<Integer,Integer> m, int k) {
        java.util.Map<Integer,Integer> out = ((java.util.Map<Integer,Integer>)(new java.util.LinkedHashMap<Integer, Integer>()));
        for (int key : m.keySet()) {
            if (((Number)(key)).intValue() != k) {
out.put(key, (int)(((int)(m).getOrDefault(key, 0))));
            }
        }
        return out;
    }

    static java.util.Map<Integer,Integer> decrement_indices(java.util.Map<Integer,Integer> m, int start) {
        java.util.Map<Integer,Integer> out_1 = ((java.util.Map<Integer,Integer>)(new java.util.LinkedHashMap<Integer, Integer>()));
        for (int key : m.keySet()) {
            int idx = (int)(((int)(m).getOrDefault(key, 0)));
            if (idx > start) {
out_1.put(key, idx - 1);
            } else {
out_1.put(key, idx);
            }
        }
        return out_1;
    }

    static void remove_vertex(Graph g, int v) {
        if (!(Boolean)contains_vertex(g, v)) {
            throw new RuntimeException(String.valueOf("vertex does not exist"));
        }
        int idx_1 = (int)(((int)((g.vertex_to_index)).getOrDefault(v, 0)));
        int[][] new_matrix = ((int[][])(new int[][]{}));
        int i_2 = 0;
        while (i_2 < g.adj_matrix.length) {
            if (i_2 != idx_1) {
                int[] row_1 = ((int[])(g.adj_matrix[i_2]));
                int[] new_row = ((int[])(new int[]{}));
                int j_2 = 0;
                while (j_2 < row_1.length) {
                    if (j_2 != idx_1) {
                        new_row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(new_row), java.util.stream.IntStream.of(row_1[j_2])).toArray()));
                    }
                    j_2 = j_2 + 1;
                }
                new_matrix = ((int[][])(appendObj(new_matrix, new_row)));
            }
            i_2 = i_2 + 1;
        }
g.adj_matrix = new_matrix;
        java.util.Map<Integer,Integer> m = remove_key(g.vertex_to_index, v);
g.vertex_to_index = decrement_indices(m, idx_1);
    }

    static void add_edge(Graph g, int u, int v) {
        if (!(((Boolean)(contains_vertex(g, u))) && ((Boolean)(contains_vertex(g, v))))) {
            throw new RuntimeException(String.valueOf("missing vertex"));
        }
        int i_3 = (int)(((int)((g.vertex_to_index)).getOrDefault(u, 0)));
        int j_3 = (int)(((int)((g.vertex_to_index)).getOrDefault(v, 0)));
        int[][] matrix_1 = ((int[][])(g.adj_matrix));
matrix_1[i_3][j_3] = 1;
        if (!g.directed) {
matrix_1[j_3][i_3] = 1;
        }
g.adj_matrix = matrix_1;
    }

    static void remove_edge(Graph g, int u, int v) {
        if (!(((Boolean)(contains_vertex(g, u))) && ((Boolean)(contains_vertex(g, v))))) {
            throw new RuntimeException(String.valueOf("missing vertex"));
        }
        int i_4 = (int)(((int)((g.vertex_to_index)).getOrDefault(u, 0)));
        int j_4 = (int)(((int)((g.vertex_to_index)).getOrDefault(v, 0)));
        int[][] matrix_2 = ((int[][])(g.adj_matrix));
matrix_2[i_4][j_4] = 0;
        if (!g.directed) {
matrix_2[j_4][i_4] = 0;
        }
g.adj_matrix = matrix_2;
    }

    static boolean contains_edge(Graph g, int u, int v) {
        if (!(((Boolean)(contains_vertex(g, u))) && ((Boolean)(contains_vertex(g, v))))) {
            throw new RuntimeException(String.valueOf("missing vertex"));
        }
        int i_5 = (int)(((int)((g.vertex_to_index)).getOrDefault(u, 0)));
        int j_5 = (int)(((int)((g.vertex_to_index)).getOrDefault(v, 0)));
        int[][] matrix_3 = ((int[][])(g.adj_matrix));
        return matrix_3[i_5][j_5] == 1;
    }

    static void clear_graph(Graph g) {
g.vertex_to_index = new java.util.LinkedHashMap<String, Object>();
g.adj_matrix = new Object[]{};
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            g_1 = make_graph(((int[])(new int[]{1, 2, 3})), ((int[][])(new int[][]{new int[]{1, 2}, new int[]{2, 3}})), false);
            System.out.println(_p(g_1.adj_matrix));
            System.out.println(_p(contains_edge(g_1, 1, 2)));
            System.out.println(_p(contains_edge(g_1, 2, 1)));
            remove_edge(g_1, 1, 2);
            System.out.println(_p(contains_edge(g_1, 1, 2)));
            remove_vertex(g_1, 2);
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
