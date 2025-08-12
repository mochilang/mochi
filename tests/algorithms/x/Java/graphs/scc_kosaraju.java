public class Main {

    static long[] dfs(long u, long[][] graph, boolean[] visit, long[] stack) {
        if (((Boolean)(visit[(int)(u)]))) {
            return stack;
        }
visit[(int)(u)] = true;
        for (long v : graph[(int)(u)]) {
            stack = ((long[])(dfs(v, ((long[][])(graph)), ((boolean[])(visit)), ((long[])(stack)))));
        }
        stack = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(stack), java.util.stream.LongStream.of(u)).toArray()));
        return stack;
    }

    static long[] dfs2(long u, long[][] reversed_graph, boolean[] visit, long[] component) {
        if (((Boolean)(visit[(int)(u)]))) {
            return component;
        }
visit[(int)(u)] = true;
        component = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(component), java.util.stream.LongStream.of(u)).toArray()));
        for (long v : reversed_graph[(int)(u)]) {
            component = ((long[])(dfs2(v, ((long[][])(reversed_graph)), ((boolean[])(visit)), ((long[])(component)))));
        }
        return component;
    }

    static long[][] kosaraju(long[][] graph) {
        long n = graph.length;
        long[][] reversed_graph_1 = ((long[][])(new long[][]{}));
        long i_1 = 0L;
        while (i_1 < n) {
            reversed_graph_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(reversed_graph_1), java.util.stream.Stream.of(new long[]{})).toArray(long[][]::new)));
            i_1 = i_1 + 1;
        }
        i_1 = 0;
        while (i_1 < n) {
            for (long v : graph[(int)(i_1)]) {
reversed_graph_1[(int)(v)] = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(reversed_graph_1[(int)(v)]), java.util.stream.LongStream.of(i_1)).toArray()));
            }
            i_1 = i_1 + 1;
        }
        boolean[] visit_1 = ((boolean[])(new boolean[]{}));
        i_1 = 0;
        while (i_1 < n) {
            visit_1 = ((boolean[])(appendBool(visit_1, false)));
            i_1 = i_1 + 1;
        }
        long[] stack_1 = ((long[])(new long[]{}));
        i_1 = 0;
        while (i_1 < n) {
            if (((Boolean)(visit_1[(int)(i_1)])) == false) {
                stack_1 = ((long[])(dfs(i_1, ((long[][])(graph)), ((boolean[])(visit_1)), ((long[])(stack_1)))));
            }
            i_1 = i_1 + 1;
        }
        i_1 = 0;
        while (i_1 < n) {
visit_1[(int)(i_1)] = false;
            i_1 = i_1 + 1;
        }
        long[][] scc_1 = ((long[][])(new long[][]{}));
        long idx_1 = stack_1.length - 1;
        while (idx_1 >= 0) {
            long node_1 = stack_1[(int)(idx_1)];
            if (((Boolean)(visit_1[(int)(node_1)])) == false) {
                long[] component_1 = ((long[])(new long[]{}));
                component_1 = ((long[])(dfs2(node_1, ((long[][])(reversed_graph_1)), ((boolean[])(visit_1)), ((long[])(component_1)))));
                scc_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(scc_1), java.util.stream.Stream.of(component_1)).toArray(long[][]::new)));
            }
            idx_1 = idx_1 - 1;
        }
        return scc_1;
    }

    static void main() {
        long[][] graph = ((long[][])(new long[][]{new long[]{1}, new long[]{2}, new long[]{0, 3}, new long[]{4}, new long[]{}}));
        long[][] comps_1 = ((long[][])(kosaraju(((long[][])(graph)))));
        long i_3 = 0L;
        while (i_3 < comps_1.length) {
            System.out.println(java.util.Arrays.toString(comps_1[(int)(i_3)]));
            i_3 = i_3 + 1;
        }
    }
    public static void main(String[] args) {
        main();
    }

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
