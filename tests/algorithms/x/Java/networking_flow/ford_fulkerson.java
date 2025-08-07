public class Main {
    static int INF;
    static int[][] graph;

    static boolean breadth_first_search(int[][] graph, int source, int sink, int[] parent) {
        boolean[] visited = ((boolean[])(new boolean[]{}));
        int i = 0;
        while (i < graph.length) {
            visited = ((boolean[])(appendBool(visited, false)));
            i = i + 1;
        }
        int[] queue = ((int[])(new int[]{}));
        queue = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(queue), java.util.stream.IntStream.of(source)).toArray()));
visited[source] = true;
        int head = 0;
        while (head < queue.length) {
            int u = queue[head];
            head = head + 1;
            int[] row = ((int[])(graph[u]));
            int ind = 0;
            while (ind < row.length) {
                int capacity = row[ind];
                if (visited[ind] == false && capacity > 0) {
                    queue = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(queue), java.util.stream.IntStream.of(ind)).toArray()));
visited[ind] = true;
parent[ind] = u;
                }
                ind = ind + 1;
            }
        }
        return visited[sink];
    }

    static int ford_fulkerson(int[][] graph, int source, int sink) {
        int[] parent = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < graph.length) {
            parent = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(parent), java.util.stream.IntStream.of(-1)).toArray()));
            i_1 = i_1 + 1;
        }
        int max_flow = 0;
        while (breadth_first_search(((int[][])(graph)), source, sink, ((int[])(parent)))) {
            int path_flow = INF;
            int s = sink;
            while (s != source) {
                int prev = parent[s];
                int cap = graph[prev][s];
                if (cap < path_flow) {
                    path_flow = cap;
                }
                s = prev;
            }
            max_flow = max_flow + path_flow;
            int v = sink;
            while (v != source) {
                int u_1 = parent[v];
graph[u_1][v] = graph[u_1][v] - path_flow;
graph[v][u_1] = graph[v][u_1] + path_flow;
                v = u_1;
            }
            int j = 0;
            while (j < parent.length) {
parent[j] = -1;
                j = j + 1;
            }
        }
        return max_flow;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            INF = 1000000000;
            graph = ((int[][])(new int[][]{new int[]{0, 16, 13, 0, 0, 0}, new int[]{0, 0, 10, 12, 0, 0}, new int[]{0, 4, 0, 0, 14, 0}, new int[]{0, 0, 9, 0, 0, 20}, new int[]{0, 0, 0, 7, 0, 4}, new int[]{0, 0, 0, 0, 0, 0}}));
            System.out.println(_p(ford_fulkerson(((int[][])(graph)), 0, 5)));
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
        return String.valueOf(v);
    }
}
