public class Main {

    static long[] dfs(long u, long[][] graph, boolean[] visit, long[] stack) {
        if (visit[(int)((long)(u))]) {
            return stack;
        }
visit[(int)((long)(u))] = true;
        for (long v : graph[(int)((long)(u))]) {
            stack = ((long[])(dfs((long)(v), ((long[][])(graph)), ((boolean[])(visit)), ((long[])(stack)))));
        }
        stack = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(stack), java.util.stream.LongStream.of((long)(u))).toArray()));
        return stack;
    }

    static long[] dfs2(long u, long[][] reversed_graph, boolean[] visit, long[] component) {
        if (visit[(int)((long)(u))]) {
            return component;
        }
visit[(int)((long)(u))] = true;
        component = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(component), java.util.stream.LongStream.of((long)(u))).toArray()));
        for (long v : reversed_graph[(int)((long)(u))]) {
            component = ((long[])(dfs2((long)(v), ((long[][])(reversed_graph)), ((boolean[])(visit)), ((long[])(component)))));
        }
        return component;
    }

    static long[][] kosaraju(long[][] graph) {
        long n = (long)(graph.length);
        long[][] reversed_graph_1 = ((long[][])(new long[][]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(n)) {
            reversed_graph_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(reversed_graph_1), java.util.stream.Stream.of(new long[][]{new long[]{}})).toArray(long[][]::new)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        i_1 = 0L;
        while ((long)(i_1) < (long)(n)) {
            for (long v : graph[(int)((long)(i_1))]) {
reversed_graph_1[(int)((long)(v))] = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(reversed_graph_1[(int)((long)(v))]), java.util.stream.LongStream.of((long)(i_1))).toArray()));
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        boolean[] visit_1 = ((boolean[])(new boolean[]{}));
        i_1 = 0L;
        while ((long)(i_1) < (long)(n)) {
            visit_1 = ((boolean[])(appendBool(visit_1, false)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        long[] stack_1 = ((long[])(new long[]{}));
        i_1 = 0L;
        while ((long)(i_1) < (long)(n)) {
            if ((visit_1[(int)((long)(i_1))] == false)) {
                stack_1 = ((long[])(dfs((long)(i_1), ((long[][])(graph)), ((boolean[])(visit_1)), ((long[])(stack_1)))));
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        i_1 = 0L;
        while ((long)(i_1) < (long)(n)) {
visit_1[(int)((long)(i_1))] = false;
            i_1 = (long)((long)(i_1) + 1L);
        }
        long[][] scc_1 = ((long[][])(new long[][]{}));
        long idx_1 = (long)((long)(stack_1.length) - 1L);
        while ((long)(idx_1) >= 0L) {
            long node_1 = (long)(stack_1[(int)((long)(idx_1))]);
            if ((visit_1[(int)((long)(node_1))] == false)) {
                long[] component_1 = ((long[])(new long[]{}));
                component_1 = ((long[])(dfs2((long)(node_1), ((long[][])(reversed_graph_1)), ((boolean[])(visit_1)), ((long[])(component_1)))));
                scc_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(scc_1), java.util.stream.Stream.of(new long[][]{component_1})).toArray(long[][]::new)));
            }
            idx_1 = (long)((long)(idx_1) - 1L);
        }
        return scc_1;
    }

    static void main() {
        long[][] graph = ((long[][])(new long[][]{new long[]{1}, new long[]{2}, new long[]{0, 3}, new long[]{4}, new long[]{}}));
        long[][] comps_1 = ((long[][])(kosaraju(((long[][])(graph)))));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(comps_1.length)) {
            System.out.println(java.util.Arrays.toString(comps_1[(int)((long)(i_3))]));
            i_3 = (long)((long)(i_3) + 1L);
        }
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
}
