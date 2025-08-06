public class Main {
    static class Edge {
        int to;
        int weight;
        Edge(int to, int weight) {
            this.to = to;
            this.weight = weight;
        }
        Edge() {}
        @Override public String toString() {
            return String.format("{'to': %s, 'weight': %s}", String.valueOf(to), String.valueOf(weight));
        }
    }

    static class Pair {
        int u;
        int v;
        Pair(int u, int v) {
            this.u = u;
            this.v = v;
        }
        Pair() {}
        @Override public String toString() {
            return String.format("{'u': %s, 'v': %s}", String.valueOf(u), String.valueOf(v));
        }
    }

    static int INF;
    static Edge[][] adjacency_list;
    static Pair[] mst_edges;

    static String pairs_to_string(Pair[] edges) {
        String s = "[";
        int i = 0;
        while (i < edges.length) {
            Pair e = edges[i];
            s = s + "(" + _p(e.u) + ", " + _p(e.v) + ")";
            if (i < edges.length - 1) {
                s = s + ", ";
            }
            i = i + 1;
        }
        return s + "]";
    }

    static Pair[] prim_mst(Edge[][] graph) {
        int n = graph.length;
        boolean[] visited = ((boolean[])(new boolean[]{}));
        int[] dist = ((int[])(new int[]{}));
        int[] parent = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < n) {
            visited = ((boolean[])(appendBool(visited, false)));
            dist = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(dist), java.util.stream.IntStream.of(INF)).toArray()));
            parent = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(parent), java.util.stream.IntStream.of(-1)).toArray()));
            i_1 = i_1 + 1;
        }
dist[0] = 0;
        Pair[] result = ((Pair[])(new Pair[]{}));
        int count = 0;
        while (count < n) {
            int min_val = INF;
            int u = 0;
            int v = 0;
            while (v < n) {
                if (visited[v] == false && dist[v] < min_val) {
                    min_val = dist[v];
                    u = v;
                }
                v = v + 1;
            }
            if (min_val == INF) {
                break;
            }
visited[u] = true;
            if (u != 0) {
                result = ((Pair[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(new Pair(parent[u], u))).toArray(Pair[]::new)));
            }
            for (Edge e : graph[u]) {
                if (visited[e.to] == false && e.weight < dist[e.to]) {
dist[e.to] = e.weight;
parent[e.to] = u;
                }
            }
            count = count + 1;
        }
        return result;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            INF = 1000000000;
            adjacency_list = ((Edge[][])(new Edge[][]{new Edge[]{new Edge(1, 1), new Edge(3, 3)}, new Edge[]{new Edge(0, 1), new Edge(2, 6), new Edge(3, 5), new Edge(4, 1)}, new Edge[]{new Edge(1, 6), new Edge(4, 5), new Edge(5, 2)}, new Edge[]{new Edge(0, 3), new Edge(1, 5), new Edge(4, 1)}, new Edge[]{new Edge(1, 1), new Edge(2, 5), new Edge(3, 1), new Edge(5, 4)}, new Edge[]{new Edge(2, 2), new Edge(4, 4)}}));
            mst_edges = ((Pair[])(prim_mst(((Edge[][])(adjacency_list)))));
            System.out.println(pairs_to_string(((Pair[])(mst_edges))));
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
