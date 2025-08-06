public class Main {
    static java.util.Map<Integer,int[]> test_graph_1;
    static java.util.Map<Integer,int[]> test_graph_2;

    static int[] dfs(java.util.Map<Integer,int[]> graph, int vert, boolean[] visited) {
visited[vert] = true;
        int[] connected_verts = ((int[])(new int[]{}));
        for (int neighbour : ((int[])(graph).get(vert))) {
            if (!(Boolean)visited[neighbour]) {
                connected_verts = ((int[])(concat(connected_verts, dfs(graph, neighbour, ((boolean[])(visited))))));
            }
        }
        return concat(new int[]{vert}, connected_verts);
    }

    static int[][] connected_components(java.util.Map<Integer,int[]> graph) {
        int graph_size = graph.size();
        boolean[] visited = ((boolean[])(new boolean[]{}));
        for (int _v = 0; _v < graph_size; _v++) {
            visited = ((boolean[])(appendBool(visited, false)));
        }
        int[][] components_list = ((int[][])(new int[][]{}));
        for (int i = 0; i < graph_size; i++) {
            if (!(Boolean)visited[i]) {
                int[] component = ((int[])(dfs(graph, i, ((boolean[])(visited)))));
                components_list = ((int[][])(appendObj(components_list, component)));
            }
        }
        return components_list;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            test_graph_1 = ((java.util.Map<Integer,int[]>)(new java.util.LinkedHashMap<Integer, int[]>(java.util.Map.ofEntries(java.util.Map.entry(0, ((int[])(new int[]{1, 2}))), java.util.Map.entry(1, ((int[])(new int[]{0, 3}))), java.util.Map.entry(2, ((int[])(new int[]{0}))), java.util.Map.entry(3, ((int[])(new int[]{1}))), java.util.Map.entry(4, ((int[])(new int[]{5, 6}))), java.util.Map.entry(5, ((int[])(new int[]{4, 6}))), java.util.Map.entry(6, ((int[])(new int[]{4, 5})))))));
            test_graph_2 = ((java.util.Map<Integer,int[]>)(new java.util.LinkedHashMap<Integer, int[]>(java.util.Map.ofEntries(java.util.Map.entry(0, ((int[])(new int[]{1, 2, 3}))), java.util.Map.entry(1, ((int[])(new int[]{0, 3}))), java.util.Map.entry(2, ((int[])(new int[]{0}))), java.util.Map.entry(3, ((int[])(new int[]{0, 1}))), java.util.Map.entry(4, new int[]{}), java.util.Map.entry(5, new int[]{})))));
            System.out.println(_p(connected_components(test_graph_1)));
            System.out.println(_p(connected_components(test_graph_2)));
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

    static <T> T[] concat(T[] a, T[] b) {
        T[] out = java.util.Arrays.copyOf(a, a.length + b.length);
        System.arraycopy(b, 0, out, a.length, b.length);
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
