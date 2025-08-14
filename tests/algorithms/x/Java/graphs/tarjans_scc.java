public class Main {

    static long[][] tarjan(long[][] g) {
        long n = (long)(g.length);
        long[][] stack_1 = new long[1][];
        stack_1[0] = ((long[])(new long[]{}));
        boolean[][] on_stack_1 = new boolean[1][];
        on_stack_1[0] = ((boolean[])(new boolean[]{}));
        long[][] index_of_1 = new long[1][];
        index_of_1[0] = ((long[])(new long[]{}));
        long[][] lowlink_of_1 = new long[1][];
        lowlink_of_1[0] = ((long[])(new long[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(n)) {
            on_stack_1[0] = ((boolean[])(appendBool(on_stack_1[0], false)));
            index_of_1[0] = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(index_of_1[0]), java.util.stream.LongStream.of((long)(0L - 1L))).toArray()));
            lowlink_of_1[0] = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(lowlink_of_1[0]), java.util.stream.LongStream.of((long)(0L - 1L))).toArray()));
            i_1 = (long)((long)(i_1) + 1L);
        }
        long[][][] components_1 = new long[1][][];
        components_1[0] = ((long[][])(new long[][]{}));
        java.util.function.BiFunction<Long,Long,Long>[] strong_connect = new java.util.function.BiFunction[1];
        strong_connect[0] = (v, index) -> {
index_of_1[0][(int)((long)(v))] = (long)(index);
lowlink_of_1[0][(int)((long)(v))] = (long)(index);
        long current_index_1 = (long)((long)(index) + 1L);
        stack_1[0] = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(stack_1[0]), java.util.stream.LongStream.of((long)(v))).toArray()));
on_stack_1[0][(int)((long)(v))] = true;
        for (long w : g[(int)((long)(v))]) {
            if ((long)(index_of_1[0][(int)((long)(w))]) == (long)(0L - 1L)) {
                current_index_1 = ((Number)(strong_connect[0].apply((long)(w), (long)(current_index_1)))).longValue();
                if ((long)(lowlink_of_1[0][(int)((long)(w))]) < (long)(lowlink_of_1[0][(int)((long)(v))])) {
lowlink_of_1[0][(int)((long)(v))] = (long)(lowlink_of_1[0][(int)((long)(w))]);
                }
            } else             if (on_stack_1[0][(int)((long)(w))]) {
                if ((long)(lowlink_of_1[0][(int)((long)(w))]) < (long)(lowlink_of_1[0][(int)((long)(v))])) {
lowlink_of_1[0][(int)((long)(v))] = (long)(lowlink_of_1[0][(int)((long)(w))]);
                }
            }
        }
        if ((long)(lowlink_of_1[0][(int)((long)(v))]) == (long)(index_of_1[0][(int)((long)(v))])) {
            long[] component_1 = ((long[])(new long[]{}));
            long w_2 = (long)(stack_1[0][(int)((long)((long)(stack_1[0].length) - 1L))]);
            stack_1[0] = ((long[])(java.util.Arrays.copyOfRange(stack_1[0], (int)((long)(0)), (int)((long)((long)(stack_1[0].length) - 1L)))));
on_stack_1[0][(int)((long)(w_2))] = false;
            component_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(component_1), java.util.stream.LongStream.of((long)(w_2))).toArray()));
            while ((long)(w_2) != (long)(v)) {
                w_2 = (long)(stack_1[0][(int)((long)((long)(stack_1[0].length) - 1L))]);
                stack_1[0] = ((long[])(java.util.Arrays.copyOfRange(stack_1[0], (int)((long)(0)), (int)((long)((long)(stack_1[0].length) - 1L)))));
on_stack_1[0][(int)((long)(w_2))] = false;
                component_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(component_1), java.util.stream.LongStream.of((long)(w_2))).toArray()));
            }
            components_1[0] = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(components_1[0]), java.util.stream.Stream.of(new long[][]{component_1})).toArray(long[][]::new)));
        }
        return current_index_1;
};
        long v_1 = 0L;
        while ((long)(v_1) < (long)(n)) {
            if ((long)(index_of_1[0][(int)((long)(v_1))]) == (long)(0L - 1L)) {
                strong_connect[0].apply((long)(v_1), 0L);
            }
            v_1 = (long)((long)(v_1) + 1L);
        }
        return components_1[0];
    }

    static long[][] create_graph(long n, long[][] edges) {
        long[][] g = ((long[][])(new long[][]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(n)) {
            g = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(g), java.util.stream.Stream.of(new long[][]{new long[]{}})).toArray(long[][]::new)));
            i_3 = (long)((long)(i_3) + 1L);
        }
        for (long[] e : edges) {
            long u_1 = (long)(e[(int)((long)(0))]);
            long v_3 = (long)(e[(int)((long)(1))]);
g[(int)((long)(u_1))] = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(g[(int)((long)(u_1))]), java.util.stream.LongStream.of((long)(v_3))).toArray()));
        }
        return g;
    }

    static void main() {
        long n_vertices = 7L;
        long[] source_1 = ((long[])(new long[]{0, 0, 1, 2, 3, 3, 4, 4, 6}));
        long[] target_1 = ((long[])(new long[]{1, 3, 2, 0, 1, 4, 5, 6, 5}));
        long[][] edges_1 = ((long[][])(new long[][]{}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(source_1.length)) {
            edges_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(edges_1), java.util.stream.Stream.of(new long[][]{new long[]{source_1[(int)((long)(i_5))], target_1[(int)((long)(i_5))]}})).toArray(long[][]::new)));
            i_5 = (long)((long)(i_5) + 1L);
        }
        long[][] g_2 = ((long[][])(create_graph((long)(n_vertices), ((long[][])(edges_1)))));
        System.out.println(_p(tarjan(((long[][])(g_2)))));
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
