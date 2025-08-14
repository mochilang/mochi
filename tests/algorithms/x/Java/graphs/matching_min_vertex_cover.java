public class Main {
    static java.util.Map<Long,long[]> graph;
    static long[] cover;

    static boolean contains(long[] xs, long v) {
        for (long x : xs) {
            if ((long)(x) == (long)(v)) {
                return true;
            }
        }
        return false;
    }

    static long[][] get_edges(java.util.Map<Long,long[]> graph) {
        long n = (long)(graph.size());
        long[][] edges_1 = ((long[][])(new long[][]{}));
        for (int i = 0; i < n; i++) {
            for (long j : ((long[])(graph).get(i))) {
                edges_1 = java.util.stream.Stream.concat(java.util.Arrays.stream(edges_1), java.util.stream.Stream.of(new Object[][]{new double[]{i, j}})).toArray(Object[][]::new);
            }
        }
        return ((long[][])(edges_1));
    }

    static long[] matching_min_vertex_cover(java.util.Map<Long,long[]> graph) {
        long[] chosen = ((long[])(new long[]{}));
        long[][] edges_3 = ((long[][])(get_edges(graph)));
        while ((long)(edges_3.length) > 0L) {
            long idx_1 = (long)((long)(edges_3.length) - 1L);
            long[] e_1 = ((long[])(edges_3[(int)((long)(idx_1))]));
            edges_3 = ((long[][])(java.util.Arrays.copyOfRange(edges_3, (int)((long)(0)), (int)((long)(idx_1)))));
            long u_1 = (long)(e_1[(int)((long)(0))]);
            long v_1 = (long)(e_1[(int)((long)(1))]);
            if (!(Boolean)contains(((long[])(chosen)), (long)(u_1))) {
                chosen = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(chosen), java.util.stream.LongStream.of((long)(u_1))).toArray()));
            }
            if (!(Boolean)contains(((long[])(chosen)), (long)(v_1))) {
                chosen = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(chosen), java.util.stream.LongStream.of((long)(v_1))).toArray()));
            }
            long[][] filtered_1 = ((long[][])(new long[][]{}));
            for (long[] edge : edges_3) {
                long a_1 = (long)(edge[(int)((long)(0))]);
                long b_1 = (long)(edge[(int)((long)(1))]);
                if ((long)(a_1) != (long)(u_1) && (long)(b_1) != (long)(u_1) && (long)(a_1) != (long)(v_1) && (long)(b_1) != (long)(v_1)) {
                    filtered_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(filtered_1), java.util.stream.Stream.of(new long[][]{edge})).toArray(long[][]::new)));
                }
            }
            edges_3 = ((long[][])(filtered_1));
        }
        return chosen;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            graph = ((java.util.Map<Long,long[]>)(new java.util.LinkedHashMap<Long, long[]>(java.util.Map.ofEntries(java.util.Map.entry(0L, ((long[])(new long[]{1, 3}))), java.util.Map.entry(1L, ((long[])(new long[]{0, 3}))), java.util.Map.entry(2L, ((long[])(new long[]{0, 3, 4}))), java.util.Map.entry(3L, ((long[])(new long[]{0, 1, 2}))), java.util.Map.entry(4L, ((long[])(new long[]{2, 3})))))));
            cover = ((long[])(matching_min_vertex_cover(graph)));
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
