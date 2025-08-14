public class Main {
    static class CheckResult {
        long status;
        long odd_node;
        CheckResult(long status, long odd_node) {
            this.status = status;
            this.odd_node = odd_node;
        }
        CheckResult() {}
        @Override public String toString() {
            return String.format("{'status': %s, 'odd_node': %s}", String.valueOf(status), String.valueOf(odd_node));
        }
    }

    static java.util.Map<Long,long[]> g1;
    static java.util.Map<Long,long[]> g2;
    static java.util.Map<Long,long[]> g3;
    static java.util.Map<Long,long[]> g4;
    static java.util.Map<Long,long[]> g5;
    static long max_node = 10L;

    static boolean[][] make_matrix(long n) {
        boolean[][] matrix = ((boolean[][])(new boolean[][]{}));
        long i_1 = 0L;
        while ((long)(i_1) <= (long)(n)) {
            boolean[] row_1 = ((boolean[])(new boolean[]{}));
            long j_1 = 0L;
            while ((long)(j_1) <= (long)(n)) {
                row_1 = ((boolean[])(appendBool(row_1, false)));
                j_1 = (long)((long)(j_1) + 1L);
            }
            matrix = ((boolean[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(matrix), java.util.stream.Stream.of(new boolean[][]{row_1})).toArray(boolean[][]::new)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return matrix;
    }

    static long[] dfs(long u, java.util.Map<Long,long[]> graph, boolean[][] visited_edge, long[] path) {
        path = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(path), java.util.stream.LongStream.of((long)(u))).toArray()));
        if (graph.containsKey(u)) {
            long[] neighbors_1 = (long[])(((long[])(graph).get(u)));
            long i_3 = 0L;
            while ((long)(i_3) < (long)(neighbors_1.length)) {
                long v_1 = (long)(neighbors_1[(int)((long)(i_3))]);
                if ((visited_edge[(int)((long)(u))][(int)((long)(v_1))] == false)) {
visited_edge[(int)((long)(u))][(int)((long)(v_1))] = true;
visited_edge[(int)((long)(v_1))][(int)((long)(u))] = true;
                    path = ((long[])(dfs((long)(v_1), graph, ((boolean[][])(visited_edge)), ((long[])(path)))));
                }
                i_3 = (long)((long)(i_3) + 1L);
            }
        }
        return path;
    }

    static CheckResult check_circuit_or_path(java.util.Map<Long,long[]> graph, long max_node) {
        long odd_degree_nodes = 0L;
        long odd_node_1 = (long)(-1);
        long i_5 = 0L;
        while ((long)(i_5) < (long)(max_node)) {
            if (graph.containsKey(i_5)) {
                if (Math.floorMod(((long[])(graph).get(i_5)).length, 2) == 1L) {
                    odd_degree_nodes = (long)((long)(odd_degree_nodes) + 1L);
                    odd_node_1 = (long)(i_5);
                }
            }
            i_5 = (long)((long)(i_5) + 1L);
        }
        if ((long)(odd_degree_nodes) == 0L) {
            return new CheckResult(1, odd_node_1);
        }
        if ((long)(odd_degree_nodes) == 2L) {
            return new CheckResult(2, odd_node_1);
        }
        return new CheckResult(3, odd_node_1);
    }

    static void check_euler(java.util.Map<Long,long[]> graph, long max_node) {
        boolean[][] visited_edge = ((boolean[][])(make_matrix((long)(max_node))));
        CheckResult res_1 = check_circuit_or_path(graph, (long)(max_node));
        if ((long)(res_1.status) == 3L) {
            System.out.println("graph is not Eulerian");
            System.out.println("no path");
            return;
        }
        long start_node_1 = 1L;
        if ((long)(res_1.status) == 2L) {
            start_node_1 = (long)(res_1.odd_node);
            System.out.println("graph has a Euler path");
        }
        if ((long)(res_1.status) == 1L) {
            System.out.println("graph has a Euler cycle");
        }
        long[] path_1 = ((long[])(dfs((long)(start_node_1), graph, ((boolean[][])(visited_edge)), ((long[])(new long[]{})))));
        System.out.println(_p(path_1));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            g1 = ((java.util.Map<Long,long[]>)(new java.util.LinkedHashMap<Long, long[]>(java.util.Map.ofEntries(java.util.Map.entry(1L, ((long[])(new long[]{2, 3, 4}))), java.util.Map.entry(2L, ((long[])(new long[]{1, 3}))), java.util.Map.entry(3L, ((long[])(new long[]{1, 2}))), java.util.Map.entry(4L, ((long[])(new long[]{1, 5}))), java.util.Map.entry(5L, ((long[])(new long[]{4})))))));
            g2 = ((java.util.Map<Long,long[]>)(new java.util.LinkedHashMap<Long, long[]>(java.util.Map.ofEntries(java.util.Map.entry(1L, ((long[])(new long[]{2, 3, 4, 5}))), java.util.Map.entry(2L, ((long[])(new long[]{1, 3}))), java.util.Map.entry(3L, ((long[])(new long[]{1, 2}))), java.util.Map.entry(4L, ((long[])(new long[]{1, 5}))), java.util.Map.entry(5L, ((long[])(new long[]{1, 4})))))));
            g3 = ((java.util.Map<Long,long[]>)(new java.util.LinkedHashMap<Long, long[]>(java.util.Map.ofEntries(java.util.Map.entry(1L, ((long[])(new long[]{2, 3, 4}))), java.util.Map.entry(2L, ((long[])(new long[]{1, 3, 4}))), java.util.Map.entry(3L, ((long[])(new long[]{1, 2}))), java.util.Map.entry(4L, ((long[])(new long[]{1, 2, 5}))), java.util.Map.entry(5L, ((long[])(new long[]{4})))))));
            g4 = ((java.util.Map<Long,long[]>)(new java.util.LinkedHashMap<Long, long[]>(java.util.Map.ofEntries(java.util.Map.entry(1L, ((long[])(new long[]{2, 3}))), java.util.Map.entry(2L, ((long[])(new long[]{1, 3}))), java.util.Map.entry(3L, ((long[])(new long[]{1, 2})))))));
            g5 = ((java.util.Map<Long,long[]>)(new java.util.LinkedHashMap<Long, long[]>(java.util.Map.ofEntries(java.util.Map.entry(1L, ((long[])(new long[]{}))), java.util.Map.entry(2L, ((long[])(new long[]{})))))));
            check_euler(g1, (long)(max_node));
            check_euler(g2, (long)(max_node));
            check_euler(g3, (long)(max_node));
            check_euler(g4, (long)(max_node));
            check_euler(g5, (long)(max_node));
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
}
