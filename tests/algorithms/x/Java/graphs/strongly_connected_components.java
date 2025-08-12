public class Main {

    static long[] topology_sort(long[][] graph, long vert, boolean[] visited) {
visited[(int)(vert)] = true;
        long[] order_1 = ((long[])(new long[]{}));
        for (long neighbour : graph[(int)(vert)]) {
            if (!(Boolean)visited[(int)(neighbour)]) {
                order_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(order_1), java.util.Arrays.stream(topology_sort(((long[][])(graph)), neighbour, ((boolean[])(visited))))).toArray()));
            }
        }
        order_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(order_1), java.util.stream.LongStream.of(vert)).toArray()));
        return order_1;
    }

    static long[] find_component(long[][] graph, long vert, boolean[] visited) {
visited[(int)(vert)] = true;
        long[] comp_1 = ((long[])(new long[]{vert}));
        for (long neighbour : graph[(int)(vert)]) {
            if (!(Boolean)visited[(int)(neighbour)]) {
                comp_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(comp_1), java.util.Arrays.stream(find_component(((long[][])(graph)), neighbour, ((boolean[])(visited))))).toArray()));
            }
        }
        return comp_1;
    }

    static long[][] strongly_connected_components(long[][] graph) {
        long n = graph.length;
        boolean[] visited_1 = ((boolean[])(new boolean[]{}));
        for (int _v = 0; _v < n; _v++) {
            visited_1 = ((boolean[])(appendBool(visited_1, false)));
        }
        long[][] reversed_1 = ((long[][])(new long[][]{}));
        for (int _v = 0; _v < n; _v++) {
            reversed_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(reversed_1), java.util.stream.Stream.of(new long[]{})).toArray(long[][]::new)));
        }
        for (int i = 0; i < n; i++) {
            for (long neighbour : graph[(int)(i)]) {
reversed_1[(int)(neighbour)] = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(reversed_1[(int)(neighbour)]), java.util.stream.LongStream.of(i)).toArray()));
            }
        }
        long[] order_3 = ((long[])(new long[]{}));
        for (int i = 0; i < n; i++) {
            if (!(Boolean)visited_1[(int)(i)]) {
                order_3 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(order_3), java.util.Arrays.stream(topology_sort(((long[][])(graph)), i, ((boolean[])(visited_1))))).toArray()));
            }
        }
        visited_1 = ((boolean[])(new boolean[]{}));
        for (int _v = 0; _v < n; _v++) {
            visited_1 = ((boolean[])(appendBool(visited_1, false)));
        }
        long[][] components_1 = ((long[][])(new long[][]{}));
        long i_1 = 0L;
        while (i_1 < n) {
            long v_1 = order_3[(int)(n - i_1 - 1)];
            if (!(Boolean)visited_1[(int)(v_1)]) {
                long[] comp_3 = ((long[])(find_component(((long[][])(reversed_1)), v_1, ((boolean[])(visited_1)))));
                components_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(components_1), java.util.stream.Stream.of(comp_3)).toArray(long[][]::new)));
            }
            i_1 = i_1 + 1;
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
