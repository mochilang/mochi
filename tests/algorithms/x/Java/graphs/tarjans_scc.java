public class Main {

    static long[][] tarjan(long[][] g) {
        long n = g.length;
        long[][] stack_1 = new long[1][];
        stack_1[0] = ((long[])(new long[]{}));
        boolean[][] on_stack_1 = new boolean[1][];
        on_stack_1[0] = ((boolean[])(new boolean[]{}));
        long[][] index_of_1 = new long[1][];
        index_of_1[0] = ((long[])(new long[]{}));
        long[][] lowlink_of_1 = new long[1][];
        lowlink_of_1[0] = ((long[])(new long[]{}));
        long i_1 = 0L;
        while (i_1 < n) {
            on_stack_1[0] = ((boolean[])(appendBool(on_stack_1[0], false)));
            index_of_1[0] = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(index_of_1[0]), java.util.stream.LongStream.of(0 - 1)).toArray()));
            lowlink_of_1[0] = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(lowlink_of_1[0]), java.util.stream.LongStream.of(0 - 1)).toArray()));
            i_1 = i_1 + 1;
        }
        long[][][] components_1 = new long[1][][];
        components_1[0] = ((long[][])(new long[][]{}));
        java.util.function.BiFunction<Long,Long,Long>[] strong_connect = new java.util.function.BiFunction[1];
        strong_connect[0] = (v_1, index_1) -> {
index_of_1[0][(int)((long)(v_1))] = index_1;
lowlink_of_1[0][(int)((long)(v_1))] = index_1;
        long current_index_3 = index_1 + 1;
        stack_1[0] = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(stack_1[0]), java.util.stream.LongStream.of(v_1)).toArray()));
on_stack_1[0][(int)((long)(v_1))] = true;
        for (long w : g[(int)((long)(v_1))]) {
            if (index_of_1[0][(int)((long)(w))] == 0 - 1) {
                current_index_3 = ((Number)(strong_connect[0].apply(w, current_index_3))).intValue();
                if (lowlink_of_1[0][(int)((long)(w))] < lowlink_of_1[0][(int)((long)(v_1))]) {
lowlink_of_1[0][(int)((long)(v_1))] = lowlink_of_1[0][(int)((long)(w))];
                }
            } else             if (((Boolean)(on_stack_1[0][(int)((long)(w))]))) {
                if (lowlink_of_1[0][(int)((long)(w))] < lowlink_of_1[0][(int)((long)(v_1))]) {
lowlink_of_1[0][(int)((long)(v_1))] = lowlink_of_1[0][(int)((long)(w))];
                }
            }
        }
        if (lowlink_of_1[0][(int)((long)(v_1))] == index_of_1[0][(int)((long)(v_1))]) {
            long[] component_3 = ((long[])(new long[]{}));
            long w_3 = stack_1[0][(int)((long)(stack_1[0].length - 1))];
            stack_1[0] = ((long[])(java.util.Arrays.copyOfRange(stack_1[0], (int)((long)(0)), (int)((long)(stack_1[0].length - 1)))));
on_stack_1[0][(int)((long)(w_3))] = false;
            component_3 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(component_3), java.util.stream.LongStream.of(w_3)).toArray()));
            while (w_3 != v_1) {
                w_3 = stack_1[0][(int)((long)(stack_1[0].length - 1))];
                stack_1[0] = ((long[])(java.util.Arrays.copyOfRange(stack_1[0], (int)((long)(0)), (int)((long)(stack_1[0].length - 1)))));
on_stack_1[0][(int)((long)(w_3))] = false;
                component_3 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(component_3), java.util.stream.LongStream.of(w_3)).toArray()));
            }
            components_1[0] = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(components_1[0]), java.util.stream.Stream.of(component_3)).toArray(long[][]::new)));
        }
        return current_index_3;
};
        long v_3 = 0L;
        while (v_3 < n) {
            if (index_of_1[0][(int)((long)(v_3))] == 0 - 1) {
                strong_connect[0].apply(v_3, 0L);
            }
            v_3 = v_3 + 1;
        }
        return components_1[0];
    }

    static long[][] create_graph(long n, long[][] edges) {
        long[][] g = ((long[][])(new long[][]{}));
        long i_3 = 0L;
        while (i_3 < n) {
            g = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(g), java.util.stream.Stream.of(new long[]{})).toArray(long[][]::new)));
            i_3 = i_3 + 1;
        }
        for (long[] e : edges) {
            long u_1 = e[(int)((long)(0))];
            long v_5 = e[(int)((long)(1))];
g[(int)((long)(u_1))] = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(g[(int)((long)(u_1))]), java.util.stream.LongStream.of(v_5)).toArray()));
        }
        return g;
    }

    static void main() {
        long n_vertices = 7;
        long[] source_1 = ((long[])(new long[]{0, 0, 1, 2, 3, 3, 4, 4, 6}));
        long[] target_1 = ((long[])(new long[]{1, 3, 2, 0, 1, 4, 5, 6, 5}));
        long[][] edges_1 = ((long[][])(new long[][]{}));
        long i_5 = 0L;
        while (i_5 < source_1.length) {
            edges_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(edges_1), java.util.stream.Stream.of(new long[]{source_1[(int)((long)(i_5))], target_1[(int)((long)(i_5))]})).toArray(long[][]::new)));
            i_5 = i_5 + 1;
        }
        long[][] g_2 = ((long[][])(create_graph(n_vertices, ((long[][])(edges_1)))));
        System.out.println(_p(tarjan(((long[][])(g_2)))));
    }
    public static void main(String[] args) {
        main();
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
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
