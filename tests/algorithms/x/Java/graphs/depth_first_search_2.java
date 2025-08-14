public class Main {
    static class Graph {
        java.util.Map<Long,long[]> vertex;
        long size;
        Graph(java.util.Map<Long,long[]> vertex, long size) {
            this.vertex = vertex;
            this.size = size;
        }
        Graph() {}
        @Override public String toString() {
            return String.format("{'vertex': %s, 'size': %s}", String.valueOf(vertex), String.valueOf(size));
        }
    }

    static Graph g = null;

    static Graph add_edge(Graph g, long from_vertex, long to_vertex) {
        java.util.Map<Long,long[]> v = g.vertex;
        if (v.containsKey(from_vertex)) {
            long[] lst_1 = (long[])(((long[])(v).get(from_vertex)));
            lst_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(lst_1), java.util.stream.LongStream.of((long)(to_vertex))).toArray()));
v.put(from_vertex, ((long[])(lst_1)));
        } else {
v.put(from_vertex, ((long[])(new long[]{to_vertex})));
        }
g.vertex = v;
        if ((long)((long)(from_vertex) + 1L) > (long)(g.size)) {
g.size = (long)(from_vertex) + 1L;
        }
        if ((long)((long)(to_vertex) + 1L) > (long)(g.size)) {
g.size = (long)(to_vertex) + 1L;
        }
        return g;
    }

    static String list_to_string(long[] lst) {
        String res = "";
        long i_1 = 0L;
        while ((long)(i_1) < (long)(lst.length)) {
            res = res + _p(_geti(lst, ((Number)(i_1)).intValue()));
            if ((long)(i_1) < (long)((long)(lst.length) - 1L)) {
                res = res + " ";
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return res;
    }

    static String list_to_arrow(long[] lst) {
        String res_1 = "";
        long i_3 = 0L;
        while ((long)(i_3) < (long)(lst.length)) {
            res_1 = res_1 + _p(_geti(lst, ((Number)(i_3)).intValue()));
            if ((long)(i_3) < (long)((long)(lst.length) - 1L)) {
                res_1 = res_1 + " -> ";
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
        return res_1;
    }

    static void print_graph(Graph g) {
        System.out.println(_p(g.vertex));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(g.size)) {
            long[] edges_1 = ((long[])(new long[]{}));
            if (g.vertex.containsKey(i_5)) {
                edges_1 = (long[])(((long[])(g.vertex).get(i_5)));
            }
            String line_1 = _p(i_5) + "  ->  " + String.valueOf(list_to_arrow(((long[])(edges_1))));
            System.out.println(line_1);
            i_5 = (long)((long)(i_5) + 1L);
        }
    }

    static long[] dfs_recursive(Graph g, long start_vertex, boolean[] visited, long[] order) {
visited[(int)((long)(start_vertex))] = true;
        order = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(order), java.util.stream.LongStream.of((long)(start_vertex))).toArray()));
        if (g.vertex.containsKey(start_vertex)) {
            long[] neighbors_1 = (long[])(((long[])(g.vertex).get(start_vertex)));
            long i_7 = 0L;
            while ((long)(i_7) < (long)(neighbors_1.length)) {
                long nb_1 = (long)(neighbors_1[(int)((long)(i_7))]);
                if (!(Boolean)visited[(int)((long)(nb_1))]) {
                    order = ((long[])(dfs_recursive(g, (long)(nb_1), ((boolean[])(visited)), ((long[])(order)))));
                }
                i_7 = (long)((long)(i_7) + 1L);
            }
        }
        return order;
    }

    static long[] dfs(Graph g) {
        long n = (long)(g.size);
        boolean[] visited_1 = ((boolean[])(new boolean[]{}));
        long i_9 = 0L;
        while ((long)(i_9) < (long)(n)) {
            visited_1 = ((boolean[])(appendBool(visited_1, false)));
            i_9 = (long)((long)(i_9) + 1L);
        }
        long[] order_1 = ((long[])(new long[]{}));
        i_9 = 0L;
        while ((long)(i_9) < (long)(n)) {
            if (!(Boolean)visited_1[(int)((long)(i_9))]) {
                order_1 = ((long[])(dfs_recursive(g, (long)(i_9), ((boolean[])(visited_1)), ((long[])(order_1)))));
            }
            i_9 = (long)((long)(i_9) + 1L);
        }
        return order_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            g = new Graph(new java.util.LinkedHashMap<Long, long[]>(), 0);
            g = add_edge(g, 0L, 1L);
            g = add_edge(g, 0L, 2L);
            g = add_edge(g, 1L, 2L);
            g = add_edge(g, 2L, 0L);
            g = add_edge(g, 2L, 3L);
            g = add_edge(g, 3L, 3L);
            print_graph(g);
            System.out.println("DFS:");
            System.out.println(list_to_string(((long[])(dfs(g)))));
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

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static Long _geti(long[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
