public class Main {
    static class CheckResult {
        int status;
        int odd_node;
        CheckResult(int status, int odd_node) {
            this.status = status;
            this.odd_node = odd_node;
        }
        CheckResult() {}
        @Override public String toString() {
            return String.format("{'status': %s, 'odd_node': %s}", String.valueOf(status), String.valueOf(odd_node));
        }
    }

    static java.util.Map<Integer,int[]> g1;
    static java.util.Map<Integer,int[]> g2;
    static java.util.Map<Integer,int[]> g3;
    static java.util.Map<Integer,int[]> g4;
    static java.util.Map<Integer,int[]> g5;
    static int max_node;

    static boolean[][] make_matrix(int n) {
        boolean[][] matrix = ((boolean[][])(new boolean[][]{}));
        int i = 0;
        while (i <= n) {
            boolean[] row = ((boolean[])(new boolean[]{}));
            int j = 0;
            while (j <= n) {
                row = ((boolean[])(appendBool(row, false)));
                j = j + 1;
            }
            matrix = ((boolean[][])(appendObj(matrix, row)));
            i = i + 1;
        }
        return matrix;
    }

    static int[] dfs(int u, java.util.Map<Integer,int[]> graph, boolean[][] visited_edge, int[] path) {
        path = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(path), java.util.stream.IntStream.of(u)).toArray()));
        if (((Boolean)(graph.containsKey(u)))) {
            int[] neighbors = (int[])(((int[])(graph).get(u)));
            int i_1 = 0;
            while (i_1 < neighbors.length) {
                int v = neighbors[i_1];
                if (visited_edge[u][v] == false) {
visited_edge[u][v] = true;
visited_edge[v][u] = true;
                    path = ((int[])(dfs(v, graph, ((boolean[][])(visited_edge)), ((int[])(path)))));
                }
                i_1 = i_1 + 1;
            }
        }
        return path;
    }

    static CheckResult check_circuit_or_path(java.util.Map<Integer,int[]> graph, int max_node) {
        int odd_degree_nodes = 0;
        int odd_node = -1;
        int i_2 = 0;
        while (i_2 < max_node) {
            if (((Boolean)(graph.containsKey(i_2)))) {
                if (Math.floorMod(((int[])(graph).get(i_2)).length, 2) == 1) {
                    odd_degree_nodes = odd_degree_nodes + 1;
                    odd_node = i_2;
                }
            }
            i_2 = i_2 + 1;
        }
        if (odd_degree_nodes == 0) {
            return new CheckResult(1, odd_node);
        }
        if (odd_degree_nodes == 2) {
            return new CheckResult(2, odd_node);
        }
        return new CheckResult(3, odd_node);
    }

    static void check_euler(java.util.Map<Integer,int[]> graph, int max_node) {
        boolean[][] visited_edge = ((boolean[][])(make_matrix(max_node)));
        CheckResult res = check_circuit_or_path(graph, max_node);
        if (res.status == 3) {
            System.out.println("graph is not Eulerian");
            System.out.println("no path");
            return;
        }
        int start_node = 1;
        if (res.status == 2) {
            start_node = res.odd_node;
            System.out.println("graph has a Euler path");
        }
        if (res.status == 1) {
            System.out.println("graph has a Euler cycle");
        }
        int[] path = ((int[])(dfs(start_node, graph, ((boolean[][])(visited_edge)), ((int[])(new int[]{})))));
        System.out.println(_p(path));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            g1 = ((java.util.Map<Integer,int[]>)(new java.util.LinkedHashMap<Integer, int[]>(java.util.Map.ofEntries(java.util.Map.entry(1, ((int[])(new int[]{2, 3, 4}))), java.util.Map.entry(2, ((int[])(new int[]{1, 3}))), java.util.Map.entry(3, ((int[])(new int[]{1, 2}))), java.util.Map.entry(4, ((int[])(new int[]{1, 5}))), java.util.Map.entry(5, ((int[])(new int[]{4})))))));
            g2 = ((java.util.Map<Integer,int[]>)(new java.util.LinkedHashMap<Integer, int[]>(java.util.Map.ofEntries(java.util.Map.entry(1, ((int[])(new int[]{2, 3, 4, 5}))), java.util.Map.entry(2, ((int[])(new int[]{1, 3}))), java.util.Map.entry(3, ((int[])(new int[]{1, 2}))), java.util.Map.entry(4, ((int[])(new int[]{1, 5}))), java.util.Map.entry(5, ((int[])(new int[]{1, 4})))))));
            g3 = ((java.util.Map<Integer,int[]>)(new java.util.LinkedHashMap<Integer, int[]>(java.util.Map.ofEntries(java.util.Map.entry(1, ((int[])(new int[]{2, 3, 4}))), java.util.Map.entry(2, ((int[])(new int[]{1, 3, 4}))), java.util.Map.entry(3, ((int[])(new int[]{1, 2}))), java.util.Map.entry(4, ((int[])(new int[]{1, 2, 5}))), java.util.Map.entry(5, ((int[])(new int[]{4})))))));
            g4 = ((java.util.Map<Integer,int[]>)(new java.util.LinkedHashMap<Integer, int[]>(java.util.Map.ofEntries(java.util.Map.entry(1, ((int[])(new int[]{2, 3}))), java.util.Map.entry(2, ((int[])(new int[]{1, 3}))), java.util.Map.entry(3, ((int[])(new int[]{1, 2})))))));
            g5 = ((java.util.Map<Integer,int[]>)(new java.util.LinkedHashMap<Integer, int[]>(java.util.Map.ofEntries(java.util.Map.entry(1, new int[]{}), java.util.Map.entry(2, new int[]{})))));
            max_node = 10;
            check_euler(g1, max_node);
            check_euler(g2, max_node);
            check_euler(g3, max_node);
            check_euler(g4, max_node);
            check_euler(g5, max_node);
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
