public class Main {
    static int[][] test_graph;
    static int[][] result;

    static boolean bfs(int[][] graph, int s, int t, int[] parent) {
        boolean[] visited = ((boolean[])(new boolean[]{}));
        int i = 0;
        while (i < graph.length) {
            visited = ((boolean[])(appendBool(visited, false)));
            i = i + 1;
        }
        int[] queue = ((int[])(new int[]{s}));
        int head = 0;
visited[s] = true;
        while (head < queue.length) {
            int u = queue[head];
            head = head + 1;
            int ind = 0;
            while (ind < graph[u].length) {
                if (visited[ind] == false && graph[u][ind] > 0) {
                    queue = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(queue), java.util.stream.IntStream.of(ind)).toArray()));
visited[ind] = true;
parent[ind] = u;
                }
                ind = ind + 1;
            }
        }
        return visited[t];
    }

    static int[][] mincut(int[][] graph, int source, int sink) {
        int[][] g = ((int[][])(graph));
        int[] parent = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < g.length) {
            parent = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(parent), java.util.stream.IntStream.of(-1)).toArray()));
            i_1 = i_1 + 1;
        }
        int[][] temp = ((int[][])(new int[][]{}));
        i_1 = 0;
        while (i_1 < g.length) {
            int[] row = ((int[])(new int[]{}));
            int j = 0;
            while (j < g[i_1].length) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(g[i_1][j])).toArray()));
                j = j + 1;
            }
            temp = ((int[][])(appendObj(temp, row)));
            i_1 = i_1 + 1;
        }
        while (bfs(((int[][])(g)), source, sink, ((int[])(parent)))) {
            int path_flow = 1000000000;
            int s = sink;
            while (s != source) {
                int p = parent[s];
                int cap = g[p][s];
                if (cap < path_flow) {
                    path_flow = cap;
                }
                s = p;
            }
            int v = sink;
            while (v != source) {
                int u_1 = parent[v];
g[u_1][v] = g[u_1][v] - path_flow;
g[v][u_1] = g[v][u_1] + path_flow;
                v = u_1;
            }
        }
        int[][] res = ((int[][])(new int[][]{}));
        i_1 = 0;
        while (i_1 < g.length) {
            int j_1 = 0;
            while (j_1 < g[0].length) {
                if (g[i_1][j_1] == 0 && temp[i_1][j_1] > 0) {
                    res = ((int[][])(appendObj(res, new int[]{i_1, j_1})));
                }
                j_1 = j_1 + 1;
            }
            i_1 = i_1 + 1;
        }
        return res;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            test_graph = ((int[][])(new int[][]{new int[]{0, 16, 13, 0, 0, 0}, new int[]{0, 0, 10, 12, 0, 0}, new int[]{0, 4, 0, 0, 14, 0}, new int[]{0, 0, 9, 0, 0, 20}, new int[]{0, 0, 0, 7, 0, 4}, new int[]{0, 0, 0, 0, 0, 0}}));
            result = ((int[][])(mincut(((int[][])(test_graph)), 0, 5)));
            System.out.println(_p(result));
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
