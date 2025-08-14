public class Main {
    static java.util.Map<Long,long[]> test_graph_1;
    static java.util.Map<Long,long[]> test_graph_2;

    static long[] dfs(java.util.Map<Long,long[]> graph, long vert, boolean[] visited) {
visited[(int)((long)(vert))] = true;
        long[] connected_verts_1 = ((long[])(new long[]{}));
        for (long neighbour : ((long[])(graph).get(vert))) {
            if (!(Boolean)visited[(int)((long)(neighbour))]) {
                connected_verts_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(connected_verts_1), java.util.Arrays.stream(dfs(graph, (long)(neighbour), ((boolean[])(visited))))).toArray()));
            }
        }
        return java.util.stream.LongStream.concat(java.util.Arrays.stream(new long[]{vert}), java.util.Arrays.stream(connected_verts_1)).toArray();
    }

    static long[][] connected_components(java.util.Map<Long,long[]> graph) {
        long graph_size = (long)(graph.size());
        boolean[] visited_1 = ((boolean[])(new boolean[]{}));
        for (int _v = 0; _v < graph_size; _v++) {
            visited_1 = ((boolean[])(appendBool(visited_1, false)));
        }
        long[][] components_list_1 = ((long[][])(new long[][]{}));
        for (int i = 0; i < graph_size; i++) {
            if (!(Boolean)visited_1[(int)((long)(i))]) {
                long[] component_1 = ((long[])(dfs(graph, (long)(i), ((boolean[])(visited_1)))));
                components_list_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(components_list_1), java.util.stream.Stream.of(new long[][]{component_1})).toArray(long[][]::new)));
            }
        }
        return components_list_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            test_graph_1 = ((java.util.Map<Long,long[]>)(new java.util.LinkedHashMap<Long, long[]>(java.util.Map.ofEntries(java.util.Map.entry(0L, ((long[])(new long[]{1, 2}))), java.util.Map.entry(1L, ((long[])(new long[]{0, 3}))), java.util.Map.entry(2L, ((long[])(new long[]{0}))), java.util.Map.entry(3L, ((long[])(new long[]{1}))), java.util.Map.entry(4L, ((long[])(new long[]{5, 6}))), java.util.Map.entry(5L, ((long[])(new long[]{4, 6}))), java.util.Map.entry(6L, ((long[])(new long[]{4, 5})))))));
            test_graph_2 = ((java.util.Map<Long,long[]>)(new java.util.LinkedHashMap<Long, long[]>(java.util.Map.ofEntries(java.util.Map.entry(0L, ((long[])(new long[]{1, 2, 3}))), java.util.Map.entry(1L, ((long[])(new long[]{0, 3}))), java.util.Map.entry(2L, ((long[])(new long[]{0}))), java.util.Map.entry(3L, ((long[])(new long[]{0, 1}))), java.util.Map.entry(4L, ((long[])(new long[]{}))), java.util.Map.entry(5L, ((long[])(new long[]{})))))));
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
