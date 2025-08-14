public class Main {

    static long[] topology_sort(long[][] graph, long vert, boolean[] visited) {
visited[(int)((long)(vert))] = true;
        long[] order_1 = ((long[])(new long[]{}));
        for (long neighbour : graph[(int)((long)(vert))]) {
            if (!(Boolean)visited[(int)((long)(neighbour))]) {
                order_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(order_1), java.util.Arrays.stream(topology_sort(((long[][])(graph)), (long)(neighbour), ((boolean[])(visited))))).toArray()));
            }
        }
        order_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(order_1), java.util.stream.LongStream.of((long)(vert))).toArray()));
        return order_1;
    }

    static long[] find_component(long[][] graph, long vert, boolean[] visited) {
visited[(int)((long)(vert))] = true;
        long[] comp_1 = ((long[])(new long[]{vert}));
        for (long neighbour : graph[(int)((long)(vert))]) {
            if (!(Boolean)visited[(int)((long)(neighbour))]) {
                comp_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(comp_1), java.util.Arrays.stream(find_component(((long[][])(graph)), (long)(neighbour), ((boolean[])(visited))))).toArray()));
            }
        }
        return comp_1;
    }

    static long[][] strongly_connected_components(long[][] graph) {
        long n = (long)(graph.length);
        boolean[] visited_1 = ((boolean[])(new boolean[]{}));
        for (int _v = 0; _v < n; _v++) {
            visited_1 = ((boolean[])(appendBool(visited_1, false)));
        }
        long[][] reversed_1 = ((long[][])(new long[][]{}));
        for (int _v = 0; _v < n; _v++) {
            reversed_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(reversed_1), java.util.stream.Stream.of(new long[][]{new long[]{}})).toArray(long[][]::new)));
        }
        for (int i = 0; i < n; i++) {
            for (long neighbour : graph[(int)((long)(i))]) {
reversed_1[(int)((long)(neighbour))] = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(reversed_1[(int)((long)(neighbour))]), java.util.stream.LongStream.of((long)(i))).toArray()));
            }
        }
        long[] order_3 = ((long[])(new long[]{}));
        for (int i = 0; i < n; i++) {
            if (!(Boolean)visited_1[(int)((long)(i))]) {
                order_3 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(order_3), java.util.Arrays.stream(topology_sort(((long[][])(graph)), (long)(i), ((boolean[])(visited_1))))).toArray()));
            }
        }
        visited_1 = ((boolean[])(new boolean[]{}));
        for (int _v = 0; _v < n; _v++) {
            visited_1 = ((boolean[])(appendBool(visited_1, false)));
        }
        long[][] components_1 = ((long[][])(new long[][]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(n)) {
            long v_1 = (long)(order_3[(int)((long)((long)((long)(n) - (long)(i_1)) - 1L))]);
            if (!(Boolean)visited_1[(int)((long)(v_1))]) {
                long[] comp_3 = ((long[])(find_component(((long[][])(reversed_1)), (long)(v_1), ((boolean[])(visited_1)))));
                components_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(components_1), java.util.stream.Stream.of(new long[][]{comp_3})).toArray(long[][]::new)));
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return components_1;
    }

    static void main() {
        long[][] test_graph_1 = ((long[][])(new long[][]{new long[]{2, 3}, new long[]{0}, new long[]{1}, new long[]{4}, new long[]{}}));
        long[][] test_graph_2_1 = ((long[][])(new long[][]{new long[]{1, 2, 3}, new long[]{2}, new long[]{0}, new long[]{4}, new long[]{5}, new long[]{3}}));
        System.out.println(_p(strongly_connected_components(((long[][])(test_graph_1)))));
        System.out.println(_p(strongly_connected_components(((long[][])(test_graph_2_1)))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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
