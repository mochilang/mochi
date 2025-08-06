public class Main {
    static java.util.Map<Integer,int[]> graph;
    static int[] cover;

    static boolean contains(int[] xs, int v) {
        for (int x : xs) {
            if (x == v) {
                return true;
            }
        }
        return false;
    }

    static int[][] get_edges(java.util.Map<Integer,int[]> graph) {
        int n = graph.size();
        int[][] edges = ((int[][])(new int[][]{}));
        for (int i = 0; i < n; i++) {
            for (int j : ((int[])(graph).get(i))) {
                edges = ((int[][])(appendObj(edges, new int[]{i, j})));
            }
        }
        return edges;
    }

    static int[] matching_min_vertex_cover(java.util.Map<Integer,int[]> graph) {
        int[] chosen = ((int[])(new int[]{}));
        int[][] edges_1 = ((int[][])(get_edges(graph)));
        while (edges_1.length > 0) {
            int idx = edges_1.length - 1;
            int[] e = ((int[])(edges_1[idx]));
            edges_1 = ((int[][])(java.util.Arrays.copyOfRange(edges_1, 0, idx)));
            int u = e[0];
            int v = e[1];
            if (!(Boolean)contains(((int[])(chosen)), u)) {
                chosen = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(chosen), java.util.stream.IntStream.of(u)).toArray()));
            }
            if (!(Boolean)contains(((int[])(chosen)), v)) {
                chosen = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(chosen), java.util.stream.IntStream.of(v)).toArray()));
            }
            int[][] filtered = ((int[][])(new int[][]{}));
            for (int[] edge : edges_1) {
                int a = edge[0];
                int b = edge[1];
                if (a != u && b != u && a != v && b != v) {
                    filtered = ((int[][])(appendObj(filtered, edge)));
                }
            }
            edges_1 = ((int[][])(filtered));
        }
        return chosen;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            graph = ((java.util.Map<Integer,int[]>)(new java.util.LinkedHashMap<Integer, int[]>(java.util.Map.ofEntries(java.util.Map.entry(0, ((int[])(new int[]{1, 3}))), java.util.Map.entry(1, ((int[])(new int[]{0, 3}))), java.util.Map.entry(2, ((int[])(new int[]{0, 3, 4}))), java.util.Map.entry(3, ((int[])(new int[]{0, 1, 2}))), java.util.Map.entry(4, ((int[])(new int[]{2, 3})))))));
            cover = ((int[])(matching_min_vertex_cover(graph)));
            System.out.println(_p(cover));
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
