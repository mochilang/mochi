public class Main {

    static int[] dfs(int u, int[][] graph, boolean[] visit, int[] stack) {
        if (((Boolean)(visit[u]))) {
            return stack;
        }
visit[u] = true;
        for (int v : graph[u]) {
            stack = ((int[])(dfs(v, ((int[][])(graph)), ((boolean[])(visit)), ((int[])(stack)))));
        }
        stack = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(stack), java.util.stream.IntStream.of(u)).toArray()));
        return stack;
    }

    static int[] dfs2(int u, int[][] reversed_graph, boolean[] visit, int[] component) {
        if (((Boolean)(visit[u]))) {
            return component;
        }
visit[u] = true;
        component = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(component), java.util.stream.IntStream.of(u)).toArray()));
        for (int v : reversed_graph[u]) {
            component = ((int[])(dfs2(v, ((int[][])(reversed_graph)), ((boolean[])(visit)), ((int[])(component)))));
        }
        return component;
    }

    static int[][] kosaraju(int[][] graph) {
        int n = graph.length;
        int[][] reversed_graph = ((int[][])(new int[][]{}));
        int i = 0;
        while (i < n) {
            reversed_graph = ((int[][])(appendObj(reversed_graph, new int[]{})));
            i = i + 1;
        }
        i = 0;
        while (i < n) {
            for (int v : graph[i]) {
reversed_graph[v] = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(reversed_graph[v]), java.util.stream.IntStream.of(i)).toArray()));
            }
            i = i + 1;
        }
        boolean[] visit = ((boolean[])(new boolean[]{}));
        i = 0;
        while (i < n) {
            visit = ((boolean[])(appendBool(visit, false)));
            i = i + 1;
        }
        int[] stack = ((int[])(new int[]{}));
        i = 0;
        while (i < n) {
            if (visit[i] == false) {
                stack = ((int[])(dfs(i, ((int[][])(graph)), ((boolean[])(visit)), ((int[])(stack)))));
            }
            i = i + 1;
        }
        i = 0;
        while (i < n) {
visit[i] = false;
            i = i + 1;
        }
        int[][] scc = ((int[][])(new int[][]{}));
        int idx = stack.length - 1;
        while (idx >= 0) {
            int node = stack[idx];
            if (visit[node] == false) {
                int[] component = ((int[])(new int[]{}));
                component = ((int[])(dfs2(node, ((int[][])(reversed_graph)), ((boolean[])(visit)), ((int[])(component)))));
                scc = ((int[][])(appendObj(scc, component)));
            }
            idx = idx - 1;
        }
        return scc;
    }

    static void main() {
        int[][] graph = ((int[][])(new int[][]{new int[]{1}, new int[]{2}, new int[]{0, 3}, new int[]{4}, new int[]{}}));
        int[][] comps = ((int[][])(kosaraju(((int[][])(graph)))));
        int i_1 = 0;
        while (i_1 < comps.length) {
            System.out.println(java.util.Arrays.toString(comps[i_1]));
            i_1 = i_1 + 1;
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
