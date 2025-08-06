public class Main {
    static class Edge {
        int node;
        int weight;
        Edge(int node, int weight) {
            this.node = node;
            this.weight = weight;
        }
        Edge() {}
        @Override public String toString() {
            return String.format("{'node': %s, 'weight': %s}", String.valueOf(node), String.valueOf(weight));
        }
    }

    static Edge[][] graph;
    static int[] dist_1;

    static int[] make_int_list(int n, int value) {
        int[] lst = ((int[])(new int[]{}));
        int i = 0;
        while (i < n) {
            lst = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(lst), java.util.stream.IntStream.of(value)).toArray()));
            i = i + 1;
        }
        return lst;
    }

    static boolean[] make_bool_list(int n) {
        boolean[] lst_1 = ((boolean[])(new boolean[]{}));
        int i_1 = 0;
        while (i_1 < n) {
            lst_1 = ((boolean[])(appendBool(lst_1, false)));
            i_1 = i_1 + 1;
        }
        return lst_1;
    }

    static int[] dijkstra(Edge[][] graph, int src) {
        int n = graph.length;
        int[] dist = ((int[])(make_int_list(n, 1000000000)));
        boolean[] visited = ((boolean[])(make_bool_list(n)));
dist[src] = 0;
        int count = 0;
        while (count < n) {
            int u = -1;
            int min_dist = 1000000000;
            int i_2 = 0;
            while (i_2 < n) {
                if (!visited[i_2] && dist[i_2] < min_dist) {
                    min_dist = dist[i_2];
                    u = i_2;
                }
                i_2 = i_2 + 1;
            }
            if (u < 0) {
                break;
            }
visited[u] = true;
            int j = 0;
            while (j < graph[u].length) {
                Edge e = graph[u][j];
                int v = e.node;
                int w = e.weight;
                if (!visited[v]) {
                    int new_dist = dist[u] + w;
                    if (new_dist < dist[v]) {
dist[v] = new_dist;
                    }
                }
                j = j + 1;
            }
            count = count + 1;
        }
        return dist;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            graph = ((Edge[][])(new Edge[][]{new Edge[]{new Edge(1, 10), new Edge(3, 5)}, new Edge[]{new Edge(2, 1), new Edge(3, 2)}, new Edge[]{new Edge(4, 4)}, new Edge[]{new Edge(1, 3), new Edge(2, 9), new Edge(4, 2)}, new Edge[]{new Edge(0, 7), new Edge(2, 6)}}));
            dist_1 = ((int[])(dijkstra(((Edge[][])(graph)), 0)));
            System.out.println(_p(_geti(dist_1, 0)));
            System.out.println(_p(_geti(dist_1, 1)));
            System.out.println(_p(_geti(dist_1, 2)));
            System.out.println(_p(_geti(dist_1, 3)));
            System.out.println(_p(_geti(dist_1, 4)));
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
